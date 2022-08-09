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

   procedure Initialize
   is
      Internal_Detected : Boolean;
      function SHOTPLUG_CTL_DDI_HPD_ENABLE (Port : TGL_Digital_Port) return Word32 is
      (case Port is
       when DIGI_A => 8 * 2 ** 0,
       when DIGI_B => 8 * 2 ** 4,
       when DIGI_C => 8 * 2 ** 8,
       when others => 0);
   begin
      pragma Debug (Debug.Put_Line (GNAT.Source_Info.Enclosing_Entity));
      -- only for DDI_A :-(
      if Config.Has_Presence_Straps and not Config.Ignore_Presence_Straps then
         Registers.Is_Set_Mask
           (Register => Registers.DDI_BUF_CTL_A,
            Mask => 1 * 2 ** 0,
            Result => Internal_Detected);
      else
         Internal_Detected := False;
      end if;

      -- for type-c, hotplug can take 3 paths:
      --   1) legacy: comes from south display hotplug detection
      --   2) thunderbolt: north display thunderbolt hotplug control/irq
      --   3) DP alt mode: north display typeC hotplug control/irq


      -- enable hotplug
--      Registers.Write
--        (Register => GEN11_DE_HPD_IMR,
--	 Value => 0); --- type-C !!!!
      --Registers.Posting_Read (Registers.GEN11_DE_HPD_IMR);

      -- clearing bits in IMR selects which interrupts are reported
      -- in IIR (set bits must be cleared by software). IER is used
      -- to enable CPU interrupts
      -- unset_and_set on SDEIMR (DDI port bits are 1<<(port + 16)
      Registers.Unset_Mask
        (Register => Registers.SDEIMR,
	 Mask => 16#7_0000#);
      Registers.Posting_Read (Registers.SDEIMR);

      if Internal_Detected then
         Registers.Unset_And_Set_Mask
           (Register => Registers.SHOTPLUG_CTL,
               -- clear the long/short detect bits
               Mask_Unset  => SHOTPLUG_CTL_DETECT_MASK,
               Mask_Set    => SHOTPLUG_CTL_HPD_INPUT_ENABLE (DIGI_A) or
                              SHOTPLUG_CTL_HPD_STATUS (DIGI_A));
      end if;

      -- for hotplug, check GEN11_DISPLAY_INT_CTL for GEN11_DE_HPD_IRQ bit
      -- if set, then check SHOTPLUG_CTL_DDI for set bits (long detect = 2 << (4 * port))
      --Time.U_Delay (10_000);
      --Registers.Read (SHOTPLUG_CTL,     

      Config.Valid_Port (eDP) := Internal_Detected;
      Config.Valid_Port (HDMI2) := True;
   end Initialize;

   procedure Hotplug_Detect
     (Port     : in Active_Port_Type;
      Detected : out Boolean)
   is
      Ctl : Word32;
   begin
      pragma Debug (Debug.Put_Line (GNAT.Source_Info.Enclosing_Entity));

      Registers.Read (Registers.SHOTPLUG_CTL, Ctl);
      Debug.Put ("SHOTPLUG_CTL :");
      Debug.Put_Word32 (Ctl);
      Debug.New_Line;

      -- TC COLD must be blocked before Type-C registers
      -- can be accessed.
      if (Port = eDP) then
         Detected := Config.Valid_Port (eDP);
      elsif Port = HDMI2 then
         Registers.Read (Registers.SHOTPLUG_CTL, Ctl);
	 Debug.Put ("SHOTPLUG_CTL :");
	 Debug.Put_Word32 (Ctl);
	 Debug.New_Line;
         Config.Valid_Port (HDMI2) := True;
         Detected := True;
      else
         Detected := False;
      end if;

   end Hotplug_Detect;

   procedure Clear_Hotplug_Detect (Port : Active_Port_Type)
   is
   begin
      pragma Debug (Debug.Put_Line (GNAT.Source_Info.Enclosing_Entity));
   end Clear_Hotplug_Detect;

end HW.GFX.GMA.Port_Detect;
