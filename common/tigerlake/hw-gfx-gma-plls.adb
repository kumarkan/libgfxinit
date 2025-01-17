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

with HW.Debug;
with GNAT.Source_Info;
with HW.GFX.GMA.PLLs.Combo_Phy;
with HW.GFX.GMA.Config;

package body HW.GFX.GMA.PLLs
with
   Refined_State => (State => PLLs)
is

   type PLL_Type is (PLL_Unknown, PLL_Combo_Phy, PLL_DKL);
   function Port_PLL_Type (Port : GPU_Port) return PLL_Type is
     (case Port is
         when DIGI_A | DIGI_B | DIGI_C => PLL_Combo_Phy,
         when DDI_TC1 | DDI_TC2 | DDI_TC3 | DDI_TC4 | DDI_TC5 | DDI_TC6 => PLL_DKL,
         when others => PLL_Unknown);

   type Count_Range is new Natural range 0 .. 2;
   type PLL_State is record
      Use_Count  : Count_Range;
      Used_For_DP : Boolean;
      Link_Rate   : DP_Bandwidth;
      Mode        : Mode_Type;
   end record;

   type PLL_State_Array is array (Configurable_DPLLs) of PLL_State;
   PLLs : PLL_State_Array;

   procedure Initialize is
   begin
      pragma Debug (Debug.Put_Line (GNAT.Source_Info.Enclosing_Entity));
      PLLs :=
        (Configurable_DPLLs =>
           (Use_Count   => 0,
            Used_For_DP => False,
            Link_Rate   => DP_Bandwidth'First,
            Mode        => Invalid_Mode));
   end Initialize;

   procedure Alloc
     (Port_Cfg : in     Port_Config;
      PLL      :    out T;
      Success  :    out Boolean)
   is
      function Config_Matches (PE : HW.GFX.GMA.PLLs.PLL_State) return Boolean
      is
      begin
         return
            PE.Used_For_DP = (Port_Cfg.Display = DP) and
            ((PE.Used_For_DP and PE.Link_Rate = Port_Cfg.DP.Bandwidth) or
             (not PE.Used_For_DP and PE.Mode = Port_Cfg.Mode));
      end Config_Matches;
   begin
      pragma Debug (Debug.Put_Line (GNAT.Source_Info.Enclosing_Entity));

      for P in Configurable_DPLLs loop
         Success := PLLs (P).Use_Count /= 0 and
                     PLLs (P).Use_Count /= Count_Range'Last and
                     Config_Matches (PLLs (P));
         if Success then
            PLL := P;
            PLLs (PLL).Use_Count := PLLs (PLL).Use_Count + 1;
            return;
         end if;
      end loop;

      for P in Configurable_DPLLs loop
         if PLLs (P).Use_Count = 0 then
            PLL := P;
            Combo_Phy.On (PLL, Port_Cfg, Success);
            if Success then
               PLLs (PLL) :=
                 (Use_Count   => 1,
                  Used_For_DP => Port_Cfg.Display = DP,
                  Link_Rate   => Port_Cfg.DP.Bandwidth,
                  Mode        => Port_Cfg.Mode);
            end if;
            return;
         end if;
      end loop;

      PLL := Invalid;
      Success := False;
   end Alloc;

   procedure Free (PLL : T) is
   begin
      pragma Debug (Debug.Put_Line (GNAT.Source_Info.Enclosing_Entity));

      if PLL in Configurable_DPLLs then
         Combo_Phy.Free (PLL);
      end if;
   end Free;

   procedure All_Off is
   begin
      pragma Debug (Debug.Put_Line (GNAT.Source_Info.Enclosing_Entity));

      Combo_Phy.All_Off;
   end All_Off;

   function Register_Value (PLL : T) return Word32
   is
   begin
      pragma Debug (Debug.Put_Line (GNAT.Source_Info.Enclosing_Entity));

      if PLL in Configurable_DPLLs then
            return Combo_Phy.Register_Value (PLL);
      end if;

      return 0;
   end Register_Value;
end HW.GFX.GMA.PLLs;
