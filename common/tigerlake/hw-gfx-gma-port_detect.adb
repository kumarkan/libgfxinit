--
-- Copyright (C) Google, LLC
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
with HW.GFX.GMA.Connectors.TC;

with HW.Debug;
with GNAT.Source_Info;

package body HW.GFX.GMA.Port_Detect
is
   SHOTPLUG_CTL_DETECT_MASK            : constant := 16#0303_0303#;

   type Digital_Port_Value is array (Digital_Port) of Word32;
   SHOTPLUG_CTL_HPD_INPUT_ENABLE : constant Digital_Port_Value :=
     (DIGI_B => 1 * 2 **  4,
      DIGI_C => 1 * 2 ** 12,
      DIGI_A => 1 * 2 ** 28,
      others => 0);
   SHOTPLUG_CTL_HPD_STATUS : constant Digital_Port_Value :=
     (DIGI_B => 3 * 2 ** 0,
      DIGI_C => 3 * 2 ** 4,
      DIGI_A => 3 * 2 ** 8,
      others => 0);

   function SDEISR_MASK (Port : Active_Port_Type) return Word32
   is (case Port is
       when DP1 | HDMI1 => 16#1_0000#,
       when DP2 | HDMI2 => 16#2_0000#,
       when DP3 | HDMI3 => 16#4_0000#,
       when others      => 0);

   function DE_HPD_ISR_MASK (Port : Active_Port_Type) return Word32
   is (case Port is
       when USBC1_DP | USBC1_HDMI => 16#01_0000#,
       when USBC2_DP | USBC2_HDMI => 16#02_0000#,
       when USBC3_DP | USBC3_HDMI => 16#04_0000#,
       when USBC4_DP | USBC4_HDMI => 16#08_0000#,
       when USBC5_DP | USBC5_HDMI => 16#10_0000#,
       when USBC6_DP | USBC6_HDMI => 16#20_0000#,
       when others                => 0);

   function SHOTPLUG_CTL_DDI_HPD_ENABLE (Port : Combo_Port_Type) return Word32
   is (case Port is
       when DP1 | HDMI1 => 16#008#,
       when DP2 | HDMI2 => 16#080#,
       when DP3 | HDMI3 => 16#800#);

   function SHOTPLUG_CTL_TC_DDI_HPD_ENABLE (Port : USBC_Port_Type)
   return Word32
   is (case Port is
       when USBC1_DP | USBC1_HDMI => 16#00_0008#,
       when USBC2_DP | USBC2_HDMI => 16#00_0080#,
       when USBC3_DP | USBC3_HDMI => 16#00_0800#,
       when USBC4_DP | USBC4_HDMI => 16#00_8000#,
       when USBC5_DP | USBC5_HDMI => 16#08_0000#,
       when USBC6_DP | USBC6_HDMI => 16#80_0000#);

   function TC_HOTPLUG_CTL_HPD_ENABLE (Port : USBC_Port_Type)
   return Word32
   is (SHOTPLUG_CTL_TC_DDI_HPD_ENABLE (Port));

   function SHOTPLUG_CTL_DDI_LONG_DETECT (Port: Combo_Port_Type) return Word32
   is (case Port is
       when DP1 | HDMI1 => 16#002#,
       when DP2 | HDMI2 => 16#020#,
       when DP3 | HDMI3 => 16#200#);

   function SHOTPLUG_CTL_TC_LONG_DETECT (Port : USBC_Port_Type)
   return Word32
   is (case Port is
       when USBC1_DP | USBC1_HDMI => 16#00_0002#,
       when USBC2_DP | USBC2_HDMI => 16#00_0020#,
       when USBC3_DP | USBC3_HDMI => 16#00_0200#,
       when USBC4_DP | USBC4_HDMI => 16#00_2000#,
       when USBC5_DP | USBC5_HDMI => 16#02_0000#,
       when USBC6_DP | USBC6_HDMI => 16#20_0000#);

   procedure Initialize
   is
      Internal_Detected : Boolean;
      Success : Boolean;
   begin
      pragma Debug (Debug.Put_Line (GNAT.Source_Info.Enclosing_Entity));

      -- only for DDI_A / eDP
      if Config.Has_Presence_Straps and not Config.Ignore_Presence_Straps then
         Registers.Is_Set_Mask
           (Register => Registers.DDI_BUF_CTL_A,
            Mask => 1 * 2 ** 0,
            Result => Internal_Detected);
      else
         Internal_Detected := True;
      end if;
      Config.Valid_Port (eDP) := Internal_Detected;

      Registers.Unset_And_Set_Mask
        (Register => Registers.SHPD_FILTER_CNT,
         Mask_Unset => 16#1_ffff#,
         Mask_Set   => 16#1d9#);

      -- Hotplug for combo ports
      Registers.Set_Mask
        (Register => Registers.SHOTPLUG_CTL,
         Mask     =>
            SHOTPLUG_CTL_DDI_HPD_ENABLE (DP1) or
            SHOTPLUG_CTL_DDI_HPD_ENABLE (DP2) or
            SHOTPLUG_CTL_DDI_HPD_ENABLE (DP3));

      -- Hotplug for Type-C ports in legacy mode
      Registers.Set_Mask
        (Register => Registers.SHOTPLUG_CTL_TC,
         Mask     =>
            SHOTPLUG_CTL_TC_DDI_HPD_ENABLE (USBC1_DP) or
            SHOTPLUG_CTL_TC_DDI_HPD_ENABLE (USBC2_DP) or
            SHOTPLUG_CTL_TC_DDI_HPD_ENABLE (USBC3_DP) or
            SHOTPLUG_CTL_TC_DDI_HPD_ENABLE (USBC4_DP) or
            SHOTPLUG_CTL_TC_DDI_HPD_ENABLE (USBC5_DP) or
            SHOTPLUG_CTL_TC_DDI_HPD_ENABLE (USBC6_DP));
         
      -- Hotplug for Type-C ports in DP-Alt mode
      Registers.Set_Mask
        (Register => Registers.TC_HOTPLUG_CTL,
         Mask     =>
            TC_HOTPLUG_CTL_HPD_ENABLE (USBC1_DP) or
            TC_HOTPLUG_CTL_HPD_ENABLE (USBC2_DP) or
            TC_HOTPLUG_CTL_HPD_ENABLE (USBC3_DP) or
            TC_HOTPLUG_CTL_HPD_ENABLE (USBC4_DP) or
            TC_HOTPLUG_CTL_HPD_ENABLE (USBC5_DP) or
            TC_HOTPLUG_CTL_HPD_ENABLE (USBC6_DP));

      -- Assume valid for now, power is required before hotplug can be used
      for Port in DP1 .. USBC6_HDMI loop
         Config.Valid_Port (Port) := True;
      end loop;

      -- TCCOLD should be blocked first before accessing FIA registers
      -- during hotplug and connect flows. It can be unblocked only after
      -- done accessing FIA registers and there is no longer a connection
      -- i.e., after all ports are disconnected.
      --
      -- Therefore, we just block it once at the beginning and leave it
      -- that way.
      Connectors.TC.TC_Cold_Request (Connectors.TC.Block, Success);
      if not Success then
         Debug.Put_Line ("Failed to bock TCCOLD, Type-C may not work!");
      end if;
   end Initialize;

   procedure Hotplug_Detect
     (Port     : in Active_Port_Type;
      Detected : out Boolean)
   is
   begin
      pragma Debug (Debug.Put_Line (GNAT.Source_Info.Enclosing_Entity));
      Detected := False;

      if Port = eDP then
         Detected := Config.Valid_Port (eDP);
         return;
      end if;

      -- Check live status of combo ports in SDEISR
      if Port in Combo_Port_Type then
         Registers.Is_Set_Mask
           (Register => Registers.SHOTPLUG_CTL,
            Mask     => SHOTPLUG_CTL_DDI_LONG_DETECT (Port),
            Result   => Detected);
      end if;

      -- DP-Alt HPD is in the north display hotplug registers
      -- Legacy HDMI/DP hotplug is detected through the south display registers
      if Port in USBC_Port_Type then
         declare
	    TC_Detected : Boolean;
         begin
            Registers.Is_Set_Mask
              (Register => Registers.SHOTPLUG_CTL_TC,
               Mask     => SHOTPLUG_CTL_TC_LONG_DETECT (Port),
               Result   => TC_Detected);
	    Debug.Put_Reg32 ("TC_Detected : ", Word32 (if TC_Detected then 1 else 0));

            Registers.Is_Set_Mask
              (Register => Registers.GEN11_DE_HPD_ISR,
               Mask     => DE_HPD_ISR_MASK (Port),
               Result   => Detected);
	 end;
      end if;
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
