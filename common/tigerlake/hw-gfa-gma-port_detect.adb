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
   -- HOTPLUG_CTL register is used for detecting hotplug on TBT or Type-C
   --  (DP alternate mode)
   --
   -- Enable HOTPLUG_CTL and configure interrupt related registers to
   --  receive HPD IRQs.
   -- Read PORT_TX_DFLEXDPPMS DPPMSTC, PORT_TX_DFLEXDPCSSS DPPMSTC,
   --  and PORT_TX_DFLEXDPSP TC Live State.
   -- Type-C DP Alt Mode connection pending is indicated by
   --  DPSP TC Live State = 01, DPPMS = 1, DPCSSS = 0
   
   -- Legacy HDMI/DP hotplug is detected through the south display hotplug
   --  control/interrupt registers.
   
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
   
   procedure Connect_DP 
   is
   begin
      -- 1) Block TCCold
      -- 2) Read DFLEXDPPMS.DPPMSTC; if 1, then IOM has switched
      --    the lane into DP mode, else abort
      -- 3) Set DFLEXDPCSSS.DPPMSTC to 1 (disable safe mode)
      -- 4) Read DFLEXDPSP to verify port has not become disconnected
      --    If Live State is not "HPD Connect for TypeC", abort
      -- 5) Read lane assignment from DFLEXDPSP.DPX4TXLATC
      -- 6) Issue AUX reads for EDID/DPCD.
      --    Set DDI_AUX_CTL IO Select field to legacy
      --    AUX power is controlled through PWR_WELL_CTL_AUX
      -- 7) Based on lane assignment reg and EDID, software knows
      --    the max # of lanes supported.
      -- 8) Perform DP mode set enable
      -- Note that if flow is aborted at any point, clear
      --  DFLEXDPSP.DPPMSTC, then unblock TCCold
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
         Digital_Port range DIGI_B .. DIGI_I;
      Internal_Detected : Boolean;
      DDI_Detected : Boolean;
      TypeC_Detected : Boolean;
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
               Mask_Unset  => SHOTPLUG_CTL_DETECT_MASK,
               Mask_Set    => SHOTPLUG_CTL_HPD_INPUT_ENABLE (DIGI_A) or
                              SHOTPLUG_CTL_HPD_STATUS (DIGI_A));  -- clear
      end if;
      Config.Valid_Port (eDP) := Internal_Detected;

      for Port in Ext_Digital_Port range DIGI_B .. DIGI_C loop
         if 

   end Initialize;

   end Hotplug_Detect (Port : Active_Port_Type; Detected : out Boolean)
   is
      GPU_Port : constant GMA.GPU_Port :=
         Config_Helpers.To_GPU_Port (Primary, Port);
   begin
      -- DDI_[ABC]
      if GPU_Port in DIGI_A .. DIGI_C then
         Registers.Read (Registers.SHOTPLUG_CTL, Ctl32, Verbose => False);
	 Detected := (Ctl32 and SHOTPLUG_CTL_LONG_DETECT (GPU_Port)) /= 0;

         if (Ctl32 and SHOTPLUG_CTL_HPD_STATUS (GPU_Port)) /= 0 then
	    Registers.Unset_And_Set_Mask
	      (Register => Registers.SHOTPLUG_CTL,
               Mask_Unset  => SHOTPLUG_CTL_DETECT_MASK,
               Mask_Set    => SHOTPLUG_CTL_HPD_STATUS (GPU_Port));
	 end if;
      -- TypeC_1..6
      elsif GPU_Port in DIGI_D .. DIGI_I then
         Registers.Read (Registers.TC_HOTPLUG_CTL, Ctl32);
	 Detected := (Ctl32 and TC_HOTPLUG_CTL_LONG_DETECT (GPU_Port)) /= 0;

         Registers.Set_Mask
	   (Regster => Registers.TC_HOTPLUG_CTL,
	    Mask => TC_HOTPLUG_CTL_LONG_DETECT (GPU_Port));
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
