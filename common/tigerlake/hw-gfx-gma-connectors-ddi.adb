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

with HW.Time;
with HW.GFX.DP_Training;
with HW.GFX.GMA.Config;
with HW.GFX.GMA.PCH.FDI;
with HW.GFX.GMA.PCH.Transcoder;
with HW.GFX.GMA.PCH.VGA;
with HW.GFX.GMA.DP_Info;
with HW.GFX.GMA.DP_Aux_Ch;
with HW.GFX.GMA.SPLL;
with HW.GFX.GMA.DDI_Phy;
with HW.GFX.GMA.Connectors.DDI.Buffers;

with HW.Debug;
with GNAT.Source_Info;

package body HW.GFX.GMA.Connectors.DDI is

   procedure Pre_On
     (Port_Cfg : in     Port_Config;
      PLL_Hint : in     Word32;
      Success  :    out Boolean)
   is
   begin
      pragma Debug (Debug.Put_Line (GNAT.Source_Info.Enclosing_Entity));

