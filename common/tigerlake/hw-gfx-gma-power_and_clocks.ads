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

with HW.GFX.GMA.Config_Helpers;

private package HW.GFX.GMA.Power_And_Clocks is
   procedure Pre_All_Off;
   procedure Post_All_Off;

   procedure Initialize;

   procedure Limit_Dotclocks
     (Configs           : in out Pipe_Configs;
      CDClk_Switch      : out Boolean)
   with
      Post => Config_Helpers.Stable_FB (Configs'Old, Configs);
   procedure Update_CDClk (Configs : in out Pipe_Configs)
   with
      Post => Config_Helpers.Stable_FB (Configs'Old, Configs);
   procedure Enable_CDClk;

   procedure Power_Set_To (Configs : Pipe_Configs);
   procedure Power_Up (Old_Configs, New_Configs : Pipe_Configs);
   procedure Power_Down (Old_Configs, Tmp_Configs, New_Configs : Pipe_Configs);

   procedure Get_RefClk (Refclk : out Frequency_Type);

end HW.GFX.GMA.Power_And_Clocks;
