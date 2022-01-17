-- Copyright 2021 Tim Wawrzynczak

-- GNU blah blah

with HW.GFX.GMA.Config_Helpers;

private package HW.GFX.GMA.Power_And_Clocks is
   
   procedure Pre_All_Off;
   procedure Post_All_Off;
   
   procedure Initialize;
   
   procedure Limit_Dotclocks
     (Configs		: in out Pipe_Configs;
      CDClk_Switch	: out Boolean)
   with
     Post => Config_Helpers.Stable_FB (Configs'Old, Configs);
   procedure Update_CDClk (Configs : in out Pipe_Configs)
   with
     Post => Config_Helpers.Stable_FB (Configs'Old, Configs);
   procedure Enable_CDClk is null;
   
   procedure Power_Set_To (Configs : Pipe_Configs);
   procedure Power_Up (Old_Configs, New_Configs : Pipe_Configs);
   procedure Power_Down (Old_Configs, Tmp_Configs, New_Configs : Pipe_Configs);
   
end HW.GFX.GMA.Power_And_Clocks;
