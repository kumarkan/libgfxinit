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

with HW.Debug;
with GNAT.Source_Info;

package body HW.GFX.GMA.Port_Detect
is
   SHOTPLUG_CTL_DETECT_MASK            : constant := 16#0303_0303#;

   type Digital_Port_Value is array (Digital_Port) of Word32;
   SHOTPLUG_CTL_HPD_INPUT_ENABLE : constant Digital_Port_Value :=
     (DIGI_B => 1 * 2 **  4,
      DIGI_C => 1 * 2 ** 12,
      DIGI_D => 1 * 2 ** 20,
      DIGI_A => 1 * 2 ** 28,
      others => 0);
   SHOTPLUG_CTL_HPD_STATUS : constant Digital_Port_Value :=
     (DIGI_B => 3 * 2 **  0,
      DIGI_C => 3 * 2 **  8,
      DIGI_D => 3 * 2 ** 16,
      DIGI_A => 3 * 2 ** 24,
      others => 0);
   procedure Initialize
   is
      Internal_Detected : Boolean;
   begin
      pragma Debug (Debug.Put_Line (GNAT.Source_Info.Enclosing_Entity));
      if Config.Has_Presence_Straps and not Config.Ignore_Presence_Straps then
         Registers.Is_Set_Mask
           (Register => Registers.DDI_BUF_CTL_A,
            Mask => 1 * 2 ** 0,
            Result => Internal_Detected);
      else
         Internal_Detected := False;
      end if;

      Config.Valid_Port (eDP) := Internal_Detected;

      if Internal_Detected then
         Registers.Unset_And_Set_Mask
           (Register => Registers.SHOTPLUG_CTL,
               -- clear the long/short detect bits
               Mask_Unset  => SHOTPLUG_CTL_DETECT_MASK,
               Mask_Set    => SHOTPLUG_CTL_HPD_INPUT_ENABLE (DIGI_A) or
                              SHOTPLUG_CTL_HPD_STATUS (DIGI_A));
      end if;

      Config.Valid_Port (eDP) := Internal_Detected;
   end Initialize;

   procedure Hotplug_Detect
     (Port     : in Active_Port_Type;
      Detected : out Boolean)
   is
   begin
      pragma Debug (Debug.Put_Line (GNAT.Source_Info.Enclosing_Entity));
      if (Port = eDP) then
         Detected := Config.Valid_Port (eDP);
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
