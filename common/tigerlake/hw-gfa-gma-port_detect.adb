--
-- Copyright (C) 2022 Google LLC
--
-- This program is free software; you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation; either version 2 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--

with HW.GFX.GMA.Config;
with HW.GFX.GMA.Registers;
with HW.GFX.GMA.Config_Helpers;

package body HW.GFX.GMA.Port_Detect
is
   --- 3 FIAs, base 0x163000, 0x16e000, 0x16f000
   
   type DDI_Port_Value is array (Digital_Port) of Word32;
   SHOTPLUG_CTL_DDI_LONG_DETECT : constant Digital_Port_Value :=
     (DIGI_A => 3 * 2 ** 0,
      DIGI_B => 3 * 2 ** 4,
      DIGI_C => 3 * 2 ** 8,
      others => 0);

   SHOTPLUG_CLT_DDI_HPD_ENABLE : constant Digital_Port_Value :=
     (DIGI_A => 8 * 2 ** 0,
      DIGI_B => 8 * 2 ** 4,
      DIGI_C => 8 * 2 ** 8,
      others => 0);

   SHOTPLUG_CLT_DDI_HPD_STATUS : constant Digital_Port_Value :=
     (DIGI_A => 3 * 2 ** 0,
      DIGI_B => 3 * 2 ** 4,
      DIGI_C => 3 * 2 ** 8,
      others => 0);

   SHOTPLUG_CTL_DETECT_MASK : constant Word32 := 16#0003_0303#;

   type TC_Port_Value is array (Type_C_Port) of Word32;
   SHOTPLUG_CTL_TC_LONG_DETECT : constant TC_Port_Value :=
     (TypeC_1 => 2 * 2 ** 0,
      TypeC_2 => 2 * 2 ** 4,
      TypeC_3 => 2 * 2 ** 8,
      TypeC_4 => 2 * 2 ** 12,
      others => 0);

   SHOTPLUG_CTL_TC_HPD_ENABLE : constant TC_Port_Value :=
      (TypeC_1 => 8 * 2 ** 0,
       TypeC_2 => 8 * 2 ** 4,
       TypeC_3 => 8 * 2 ** 8,
       TypeC_4 => 8 * 2 ** 12);

   SHOTPLUG_CTL_TC_LONG_DETECT : constant TC_Port_Value :=
     (TypeC_1 => 3 * 2 ** 0,
      TypeC_2 => 3 * 2 ** 4,
      TypeC_3 => 3 * 2 ** 8,
      TypeC_4 => 3 * 2 ** 12,
      others => 0);

   -- If a TypeC port has a connection, then TCCold must be unblocked
   --  before accessing FIA registers (i.e. DP connect flow).

   function Control_TCCold (Enable : in Boolean) return Boolean
   is
      use type HW.Word64;
      Timeout : constant Time.T := Time.US_From_Now (600);
      Timed_Out : Boolean := False;
      
      Reply : Word64;
      Success : Boolean := False;
      Command : Word64 := 0;
   begin
      if Enable then 
	 Command := 1;
      end if;
      
      loop
	 pragma Loop_Invariant (not Timed_Out);
	 
	 PCode.Mailbox_Write_Read
	   (MBox => 16#8000_0026#,
	    Command => Command,
	    Reply,
	    Success);
	 exit when not Success;
	 exit when (Reply and 16#1# = 16#1#);
	 
	 Timed_Out := Time.Timed_Out (Timeout);
	 exit when Timed_Out;
      end loop;
      
      return not Timed_Out;
   end Control_TCCold;
   
   function Block_TCCold return Boolean 
   is
   begin
      return Control_TCCold (False);
   end Block_TCCold;
   
   function Unblock_TCCold return Boolean
   is
   begin
      return Control_TCCold (True);
   end Unblock_TCCold;

   procedure Connect_DP (Port : TypeC_Port; Success : out Boolean; Max_Lanes : out Natural)
   is
      function Status_Complete (Port : TypeC_Port) return Boolean is
         Result : Boolean;
      begin
         Registers.Is_Set_Mask
	   (Register => PORT_TX_DFLEXDPPMS (Port),
	    Mask => DP_PHY_MODE_STATUS_COMPLETE (Port),
	    Result => Result);
	 return Result;
      end Status_Complete;

     type Status_Type is record
        DP : Boolean;
	Legacy : Boolean;
     end record;

      procedure Read_Live_Status (Port : TypeC_Port; Status : out Status_Type)
      is
         Isr : Word32;
      begin
         Registers.Read
           (Register => PORT_TX_DFLEXDPSP (Port),
            Value => Status);
	 Registers.Read
	   (Register => SDEISR,
	    Value => Isr);
	 Status.DP := (Status and TC_LIVE_STATE_TC_MASK (Port)) /= 0;
	 Status.Legacy := (Isr and HPD_PIN (Port)) /= 0;        
      end Read_Live_Status;

      Live_Status : Status_Type;

      procedure Take_Ownership (Port : TypeC_Port) is
      begin
         Register.Set_Mask
	   (Register => PORT_TX_DFLEXDPCSSS (Port),
	    Mask => DP_PHY_MODE_STATUS_NOT_SAFE (Port));
      end Take_Ownership;

      procedure Release_Ownership (Port : TypeC_Port) is
      begin
         Register.Unset_Mask
	   (Register => PORT_TX_DFLEXDPCSSS (Port),
	    Mask => DP_PHY_MODE_STATUS_NOT_SAFE (Port));
      end Release_Ownership;
   begin
      Max_Lanes := 0;

      Block_TCCold ;
      
      -- 2) Read DFLEXDPPMS.DPPMSTC; if 1, then IOM has switched
      --    the lane into DP mode, else abort
      if not Status_Complete (Port) then
         Unblock_TCCold;
      	 return;
      end if;

      -- 3) Set DFLEXDPCSSS.DPPMSTC to 1 (disable safe mode)
      Read_Live_Status (Port, Live_Status);
      if not (Live_Status.DP or else Live_Status.Legacy)
        Unblock_TCCold;
	return;
      end if;

      Take_Ownership (Port);

      -- 4) Read DFLEXDPSP to verify port has not become disconnected
      --    If Live State is not "HPD Connect for TypeC", abort
      if not Status_Is_DP (Port) then
         Unblock_TCCold;
         Release_Ownership (Port);
      	 return
      end if;

      Read_TC_Lanes (Port, Max_Lanes);
   end Connect_DP;
   
   procedure Connect_Fixed
   is
   begin
      -- 1) Block TCCold
      -- 2) Read DFLEXDPPMS.DPPMSTC, if 1 the lane is in DP mode,
      --     Otherwise abort.
      -- 3) Set DFLEXDPCSSS.DPPMSTC to 1 (disable safe mode)
      -- 4) Set PWR_WELL_CTL_AUX IO Power Request
      -- 5) Poll for PWR_WELL_CTL_AUX IO Power State to become enabled
      --     timeout after 1.5ms
      -- 6) Issue reads for EDID/DPCD
      --     Set DDI_AUX_CTL IO Select field to Legacy for DP, don't care for HDMI
      -- 7) Maximum lanes supported is known from EDID for DP, max is 4 for HDMI
      -- 8) Perform DP or HDMI mode set enable
      -- Note that if flow is aborted at any point, clear
      --  DFLEXDPSP.DPPMSTC, then unblock TCCold
   end Connect_Fixed;

   procedure Enable_Aux
   is
   begin
      -- Display must already be initialized.
      -- Enable AUX Power
      -- Disable PSR1, PSR2, GTC, DC5 and DC6
      -- Program AUX data registers
      -- Configure AUX and START transaction
      -- Wait for transaction to complete
      -- Check for errors
      -- Read AUX data register
      -- Clear status flags
   end Enable_Aux;

   procedure Initialize
   is
      subtype Ext_Digital_Port is
         Digital_Port range DIGI_B .. TypeC_4;
      Internal_Detected : Boolean;
   begin
      -- DDI_A
      if Config.Has_Presence_Straps and not Config.Ignore_Presence_Straps then
         Registers.Is_Set_Mask
           (Register => Registers.DDI_BUF_CTL,
            Mask     => DDI_PORT_DETECTED (DIGI_A),
	    Result   => Internal_Detected);
      else
         Internal_Detected := False;
      end if;

      if Internal_Detected then
         Registers.Unset_And_Set_Mask
	   (Register => Registers.SHOTPLUG_CTL,
               Mask_Unset  => SHOTPLUG_CTL_DETECT_MASK, -- clear the long/short detect bits
               Mask_Set    => SHOTPLUG_CTL_DDI_HPD_ENABLE (DIGI_A) or
                              SHOTPLUG_CTL_DDI_HPD_STATUS (DIGI_A));  -- clear
      end if;
      Config.Valid_Port (eDP) := Internal_Detected;
      -- ICL onwards does not appear to have detection registers like SFUSE_STRAP...
      Config.Valid_Port (...) := ?;
   end Initialize;

   end Hotplug_Detect (Port : Active_Port_Type; Detected : out Boolean)
   is
      GPU_Port : constant GMA.GPU_Port :=
         Config_Helpers.To_GPU_Port (Primary, Port);
   begin
      -- DDI_[ABC]
      if Is_DDI_Port (GPU_Port) then
         Registers.Read (Registers.SHOTPLUG_CTL_DDI, Ctl32, Verbose => False);
	 Detected := (Ctl32 and SHOTPLUG_CTL_DDI_LONG_DETECT (GPU_Port)) /= 0;

         if Detected /= 0 then
	    Registers.Unset_And_Set_Mask
	      (Register => Registers.SHOTPLUG_CTL_DDI,
               Mask_Unset  => SHOTPLUG_CTL_DDI_HPD_ENABLE (GPU_Port),
               Mask_Set    => SHOTPLUG_CTL_DDI_STATUS_MASK (GPU_Port));
	 end if;
      -- TypeC_1..6
      elsif Is_TypeC_Port(GPU_Port) then
         Registers.Read (Registers.SHOTPLUG_CTL_TC, Ctl32);
	 Detected := (Ctl32 and SHOTPLUG_CTL_TC_LONG_DETECT (GPU_Port)) /= 0;

         if Is_Port_Legacy (Port) -- THIS WILL HAVE TO COME FROM coreboot as an input,
	    		   	  -- usually comes from the VBT.
	       Connect_Fixed (Port);
	 else
	       Connect_DP (Port);
	 end if;

         Registers.Unset_And_Set_Mask
	   (Regster => Registers.SHOTPLUG_CTL_TC,
	    Mask_Unset => SHOTPLUG_CTL_TC_STATUS_MASK (GPU_Port),
	    Mask_Set => SHOTPLUG_CTL_TC_LONG_DETECT (GPU_Port));
      else
         Detected := False;
      end if;
      -- TODO: thunderbolt
   end Hotplug_Detect;

   procedure Clear_Hotplug_Detect (Port : Active_Port_Type)
   is
      Ignored_HPD : Boolean;
   begin
      pragma Warnings (GNATprove, Off, "unused assignment to ""Ignored_HPD""",
                       Reason => "We want to clear pending events only");
      Port_Detect.Hotplug_Detect (Port, Ignored_HPD);
      pragma Warnings (GNATprove, On, "unused assignment to ""Ignored_HPD""");
   end Clear_Hotplug_Detect;
end HW.GFX.GMA.Port_Detect;
