--
-- Copyright (C) 2022 Google, LLC
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

with HW.Debug;
with GNAT.Source_Info;

package body HW.GFX.GMA.Connectors.TC is

   TGL_PCODE_TCCOLD         : constant := 16#26#;
   TCCOLD_BLOCK_REQ         : constant := 16#00#;
   TCCOLD_UNBLOCK_REQ       : constant := 16#01#;
   TCCOLD_BLOCK_RESULT_FAIL : constant := 16#01#;

   type Fia_Regs_Record is record
      PORT_TX_DLFEXDPMLE1 : Registers.Register_Index;
      PORT_TX_DFLEXDPSP   : Registers.Register_Index;
      PORT_TX_DFLEXDPPMS  : Registers.Register_Index;
      PORT_TX_DFLEXDPCSSS : Registers.Register_Index;
      PORT_TX_DFLEXPA1    : Registers.Register_Index;
   end record;
   type Fia_Regs_Array is array (USBC_Port) of Fia_Regs_Record;
   Fia_Regs : constant Fia_Regs_Array :=
      Fia_Regs_Array'
     (DDI_TC1 =>
        (PORT_TX_DLFEXDPMLE1_FIA1,
         PORT_TX_DFLEXDPSP_FIA1,
	 PORT_TX_DFLEXDPPM_FIA1,
	 PORT_TX_DFLEXDPCSSS_FIA1,
	 PORT_TX_DFLEXPA1_FIA1),
      DDI_TC2 =>
        (PORT_TX_DLFEXDPMLE1_FIA1,
         PORT_TX_DFLEXDPSP_FIA1,
	 PORT_TX_DFLEXDPPM_FIA1,
	 PORT_TX_DFLEXDPCSSS_FIA1,
	 PORT_TX_DFLEXPA1_FIA1),
      DDI_TC3 =>
        (PORT_TX_DLFEXDPMLE1_FIA2,
         PORT_TX_DFLEXDPSP_FIA2,
	 PORT_TX_DFLEXDPPM_FIA2,
	 PORT_TX_DFLEXDPCSSS_FIA2,
	 PORT_TX_DFLEXPA1_FIA2),
      DDI_TC4 =>
        (PORT_TX_DLFEXDPMLE1_FIA2,
         PORT_TX_DFLEXDPSP_FIA2,
	 PORT_TX_DFLEXDPPM_FIA2,
	 PORT_TX_DFLEXDPCSSS_FIA2,
	 PORT_TX_DFLEXPA1_FIA2),
      DDI_TC5 =>
        (PORT_TX_DLFEXDPMLE1_FIA3,
         PORT_TX_DFLEXDPSP_FIA3,
	 PORT_TX_DFLEXDPPM_FIA3,
	 PORT_TX_DFLEXDPCSSS_FIA3,
	 PORT_TX_DFLEXPA1_FIA3),
      DDI_TC6 =>
        (PORT_TX_DLFEXDPMLE1_FIA3,
         PORT_TX_DFLEXDPSP_FIA3,
	 PORT_TX_DFLEXDPPM_FIA3,
	 PORT_TX_DFLEXDPCSSS_FIA3,
	 PORT_TX_DFLEXPA1_FIA3));

   package Rep is
      function Index (Port : USBC_Port) return natural;
   end Rep;

   package body Rep is
      function Index (Port : USBC_Port) return natural
      with
         SPARK_Mode => Off
      is
      begin
         return Port'Enum_Rep - DDI_TC1'Enum_Rep;
      end Index;
   end Rep;

   function DFLEXDPMLE1_DPMLETC_MASK (Port : USBC_Port) is
      (return 15 * 2 ** (4 * Index (Port)));
   function DLFEXDPMLE1_DPMLETC_ML0 (Port : USBC_Port) is
      (return 1 * 2 ** (4 * Index (Port)));
   function DLFEXDPMLE1_DPMLETC_ML1_0 (Port : USBC_Port) is
      (return 3 * 2 ** (4 * Index (Port)));
   function DLFEXDPMLE1_DPMLETC_ML3 (Port : USBC_Port) is
      (return 8 * 2 ** (4 * Index (Port)));
   function DLFEXDPMLE1_DPMLETC_ML3_2 (Port : USBC_Port) is
      (return 12 * 2 ** (4 * Index (Port)));
   function DLFEXDPMLE1_DPMLETC_ML3_0 (Port : USBC_Port) is
      (return 15 * 2 ** (4 * Index (Port)));

   procedure Block_TC_Cold (Success : out Boolean)
   is
   begin
      Success := False;
      for Try in 1 .. 3 loop
         PCode.Mailbox_Write_Read
           (MBox    => TGL_PCODE_TCCOLD,
            Command => TCCOLD_BLOCK_REQ,
            Reply   => Result,
            Success => Success);

         if Success then
            Success := Result and TCCOLD_BLOCK_RESULT_FAIL = 0;
         end if;

         exit when Success;

         -- Wait 1 millisecond and try again
         Time.U_Delay (1_000);
      end loop;
   end Block_TC_Cold;

   procedure Set_Lane_Count (Port : USBC_Port; Lanes : Positive)
   wth
      Pre => Lanes <= 4
   is
      DPMLETC : Word32;
      Tmp : Word32 := 0;
   begin
      Registers.Read (Fia_Regs (Port).PORT_TX_DLFEXDPMLE1, DPMLETC);
      DPMLETC := DPMLETC and not DFLEXDPMLE1_DPMLETC_MASK (Port);

      case Lanes is
      when 1 =>
         DPMLETC := DPMLETC or DFLEXDPMLE1_DPMLETC_ML0 (Port_Cfg.Port);
      when 2 =>
         DPMLETC := DPMLETC or DFLEXDPMLE1_DPMLETC_ML1_0 (Port_Cfg.Port);
      when 4 =>
         DPMLETC := DPMLETC or DFLEXDPMLE1_DPMLETC_ML3_0 (Port_Cfg.Port);
      when others =>
         Debug.Put_Line ("Unsupported lane count");
      end case;

      Registers.Write (Fia_Regs (Port).PORT_TX_DLFEXDPMLE1, DPMLETC);
   end Set_Lane_Count;

   -- icl_tc_phy_connect
   procedure Pre_PLL
     (Port_Cfg : in Port_Config;
      Success  : out Boolean)
   is
   begin
      Block_TC_Cold (Success);

      if not Success then
         Debug.Put_Line ("Failed to unblock TCCOLD");
         return;
      end if;

      -- read DFLEXDPPMS.DPPMSTC (should be '1') to indicate
      -- SoC uC has switched the lane into DP mode
      Registers.Is_Set_Mask
        (Register => Fia_Regs (Port_Cfg.Port).PORT_TX_DFLEXDPPMS,
	 Mask     => DP_PHY_MODE_STATUS_COMPLETE (Port_Cfg.Port),
	 Result   => Success);

      if not Success then
         return;
      end if;

      -- set DLFEXDPCSSS.DPPMSTC to '1' to indicate display
      -- controller is not in safe mode anymore
      Registers.Set_Mask
        (Register => Fia_Regs (Port_Cfg.Port).PORT_TX_DFLEXDPCSSS,
	 Mask     => Word32 (Rep.Index (Port_Cfg.Port)));

      -- reads DLFEXDPSP to verify port has not become disconnected
      -- if live state is 'no hpd connect for typec or tbt', abort connect flow
      -- elsif live state is 'hpd connect for typec', proceed with dp alternate connect flow
      Registers.Is_Set_Mask
        (Register => Fia_Regs (Port_Cfg.Port).PORT_TX_DFLEXDPSP,
	 Mask     => TC_LIVE_STATE_TC (Port_Cfg.Port),
	 Result   => Success);

      if not Success then
         return;         
      end if;

      -- read lane assignment from DFLEXDPSP.DPX4TXLATC register
      --Registers.Read (Fia_Regs (Port_Cfg.Port).DFLEXDPSP, Tmp);

      if Port_Cfg.Display = HDMI then
         Set_Lane_Count (4);
      else
         Set_Lane_Count (Lane_Count (Port_Cfg.DP));
      end if;

   end Pre_PLL;

end HW.GFX.GMA.Connectors.TC;
