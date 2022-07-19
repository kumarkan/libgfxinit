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
with HW.GFX.GMA.Power_And_Clocks;

package body HW.GFX.GMA.PLLs.Combo_Phy is

   type PLL_Regs is array (Configurable_DPLLs) of Registers.Registers_Index;
   DPLL_ENABLE : constant PLL_Regs := PLL_Regs'
     (DPLL0 => Registers.DPLL_0_ENABLE,
      DPLL1 => Registers.DPLL_1_ENABLE,
      DPLL4 => Registers.DPLL_4_ENABLE);
   DPLL_CFGCR0 : constant PLL_Regs := PLL_Regs'
     (DPLL0 => Registers.DPLL_0_CFGCR0,
      DPLL1 => Registers.DPLL_1_CFGCR0,
      DPLL4 => Registers.DPLL_4_CFGCR0);
   DPLL_CFGCR1 : constant PLL_Regs := PLL_Regs'
     (DPLL0 => Registers.DPLL_0_CFGCR1,
      DPLL1 => Registers.DPLL_1_CFGCR1,
      DPLL4 => Registers.DPLL_4_CFGCR1);
   DPLL_SSC : constant PLL_Regs := PLL_Regs'
     (DPLL0 => Registers.DPLL_0_SSC,
      DPLL1 => Registers.DPLL_1_SSC,
      DPLL4 => Registers.DPLL_4_SSC);

   DPLL_ENABLE_PLL_ENABLE   : constant := 1 * 2 ** 31;
   DPLL_ENABLE_PLL_LOCK     : constant := 1 * 2 ** 30;
   DPLL_ENABLE_POWER_ENABLE : constant := 1 * 2 ** 27;
   DPLL_ENABLE_POWER_STATE  : constant := 1 * 2 ** 26;
   DPLL_SSC_DP              : constant := 16#200#;

   subtype PDiv_Range is Positive range 1 .. 8;
   subtype QDiv_Range is Natural range 0 .. 255;
   subtype KDiv_Range is Positive range 1 .. 5;

   type PLL_Params is record
      DCO_Integer    : Word32;
      DCO_Fraction   : Word32;
      PDiv           : PDiv_Range;
      KDiv           : KDiv_Range;
      QDiv_Mode      : natural range 0 .. 1;
      QDiv_Ratio     : QDiv_Range;
   end record;
   type DP_PLL_Params is record
      Bandwidth      : DP_Bandwidth;
      P : PLL_Params;
   end record;
   type DP_PLL_Params_Array is array (natural range 0 .. 7) of DP_PLL_Params;

   PLL_Params_19_2MHz : constant DP_PLL_Params_Array := DP_PLL_Params_Array'
     (0 =>
       (Bandwidth   => DP_Bandwidth_5_4,
        P => (DCO_Integer  => 16#1a5#,
              DCO_Fraction => 16#7000#,
              PDiv         => 2,
              KDiv         => 1,
              QDiv_Mode    => 0,
              QDiv_Ratio   => 0)),
      1 =>
       (Bandwidth   => DP_Bandwidth_2_7,
        P => (DCO_Integer  => 16#1a5#,
              DCO_Fraction => 16#7000#,
              PDiv         => 2,
              KDiv         => 2,
              QDiv_Mode    => 0,
              QDiv_Ratio   => 0)),
      2 =>
       (Bandwidth   => DP_Bandwidth_1_62,
        P => (DCO_Integer  => 16#1a5#,
              DCO_Fraction => 16#7000#,
              PDiv         => 4,
              KDiv         => 2,
              QDiv_Mode    => 0,
              QDiv_Ratio   => 0)),
      3 =>
       (Bandwidth   => DP_Bandwidth_3_24,
        P => (DCO_Integer  => 16#1a5#,
              DCO_Fraction => 16#7000#,
              PDiv         => 4,
              KDiv         => 1,
              QDiv_Mode    => 0,
              QDiv_Ratio   => 0)),
      4 =>
       (Bandwidth   => DP_Bandwidth_2_16,
        P => (DCO_Integer  => 16#1c2#,
              DCO_Fraction => 16#0000#,
              PDiv         => 1,
              KDiv         => 2,
              QDiv_Mode    => 1,
              QDiv_Ratio   => 2)),
      5 =>
       (Bandwidth   => DP_Bandwidth_4_32,
        P => (DCO_Integer  => 16#1c2#,
              DCO_Fraction => 16#0000#,
              PDiv         => 1,
              KDiv         => 2,
              QDiv_Mode    => 0,
              QDiv_Ratio   => 0)),
      6 =>
       (Bandwidth   => DP_Bandwidth_6_48,
        P => (DCO_Integer  => 16#1fa#,
              DCO_Fraction => 16#2000#,
              PDiv         => 2,
              KDiv         => 1,
              QDiv_Mode    => 0,
              QDiv_Ratio   => 0)),
      7 =>
       (Bandwidth   => DP_Bandwidth_8_10,
        P => (DCO_Integer  => 16#1a5#,
              DCO_Fraction => 16#7000#,
              PDiv         => 1,
              KDiv         => 1,
              QDiv_Mode    => 0,
              QDiv_Ratio   => 0)));

   PLL_Params_24MHz : constant DP_PLL_Params_Array := DP_PLL_Params_Array'
     (0 =>
       (Bandwidth   => DP_Bandwidth_5_4,
        P => (DCO_Integer  => 16#151#,
              DCO_Fraction => 16#4000#,
              PDiv         => 2,
              KDiv         => 1,
              QDiv_Mode    => 0,
              QDiv_Ratio   => 0)),
      1 =>
       (Bandwidth   => DP_Bandwidth_2_7,
        P => (DCO_Integer  => 16#151#,
              DCO_Fraction => 16#4000#,
              PDiv         => 2,
              KDiv         => 2,
              QDiv_Mode    => 0,
              QDiv_Ratio   => 0)),
      2 =>
       (Bandwidth   => DP_Bandwidth_1_62,
        P => (DCO_Integer  => 16#151#,
              DCO_Fraction => 16#4000#,
              PDiv         => 2,
              KDiv         => 2,
              QDiv_Mode    => 0,
              QDiv_Ratio   => 0)),
      3 =>
       (Bandwidth   => DP_Bandwidth_3_24,
        P => (DCO_Integer  => 16#151#,
              DCO_Fraction => 16#4000#,
              PDiv         => 4,
              KDiv         => 1,
              QDiv_Mode    => 0,
              QDiv_Ratio   => 0)),
      4 =>
       (Bandwidth   => DP_Bandwidth_2_16,
        P => (DCO_Integer  => 16#168#,
              DCO_Fraction => 16#0000#,
              PDiv         => 1,
              KDiv         => 2,
              QDiv_Mode    => 1,
              QDiv_Ratio   => 2)),
      5 =>
       (Bandwidth   => DP_Bandwidth_4_32,
        P => (DCO_Integer  => 16#168#,
              DCO_Fraction => 16#0000#,
              PDiv         => 1,
              KDiv         => 2,
              QDiv_Mode    => 0,
              QDiv_Ratio   => 0)),
      6 =>
       (Bandwidth   => DP_Bandwidth_6_48,
        P => (DCO_Integer  => 16#195#,
              DCO_Fraction => 16#0000#,
              PDiv         => 2,
              KDiv         => 1,
              QDiv_Mode    => 0,
              QDiv_Ratio   => 0)),
      7 =>
       (Bandwidth   => DP_Bandwidth_8_10,
        P => (DCO_Integer  => 16#151#,
              DCO_Fraction => 16#4000#,
              PDiv         => 1,
              KDiv         => 1,
              QDiv_Mode    => 0,
              QDiv_Ratio   => 0)));

   procedure Calc_DP_PLL_Dividers (PLL       : in     T;
                                   Bandwidth : in     DP_Bandwidth;
                                   Params    :    out PLL_Params;
                                   Success   :    out Boolean)
   is
      Refclk : Frequency_Type;
   begin
      Success := False;

      Power_And_Clocks.Get_Refclk (Refclk);
      if Refclk = 24_000_000 then
         for I in PLL_Params_24MHz'Range loop
            if PLL_Params_24MHz (I).Bandwidth = Bandwidth then
               Params := PLL_Params_24MHz (I).P;
               Success := True;
               exit;
            end if;
         end loop;
      else
         for I in PLL_Params_19_2MHz'Range loop
            if PLL_Params_19_2MHz (I).Bandwidth = Bandwidth then
               Params := PLL_Params_19_2MHz (I).P;
               -- Display WA #22010492432: ehl, tgl, adl-p
               -- Program half of the nominal DCO divider fraction value
               -- for 38.4 MHz refclk
               --if Refclk = 38_400_000 then
               --   Params.DCO_Fraction := Shift_Right (Params.DCO_Fraction, 1);
               --end if;
               Success := True;
               exit;
            end if;
         end loop;
      end if;
   end Calc_DP_PLL_Dividers;

   procedure Calc_HDMI_PLL_Dividers
     (PLL      : in     T;
      Dotclock : in     Frequency_Type;
      Params   :    out PLL_Params;
      Success  :    out Boolean)
   is
      subtype Div_Range is Pos64 range 2 .. 102;
      subtype Candidate_Index is Positive range 1 .. 46;
      type Candidate_Array is array (Candidate_Index) of Div_Range;
      Candidates : constant Candidate_Array := Candidate_Array'
        (2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 24, 28, 30, 32, 36, 40, 42, 44,
         48, 50, 52, 54, 56, 60, 64, 66, 68, 70, 72, 76, 78, 80, 84, 88, 90,
         92, 96, 98, 100, 102, 3, 5, 7, 9, 15, 21);
      AFE_Clk : constant Frequency_Type := Dotclock * 5;
      DCO_Min : constant Frequency_Type := 7_998_000;
      DCO_Max : constant Frequency_Type := 10_000_000;
      DCO_Mid : constant Frequency_Type := (DCO_Min + DCO_Max) / 2;
      Best_DCO_Centrality : Frequency_Type := Frequency_Type'Last;
      Best_Div : Div_Range := Div_Range'First;
      Best_DCO : Frequency_Type;
      DCO_Found : Boolean := False;
      PDiv : PDiv_Range := PDiv_Range'First;
      QDiv : QDiv_Range := QDiv_Range'First;
      KDiv : KDiv_Range := KDiv_Range'First;
      Refclk_Freq : Frequency_Type;
   begin
      Success := False;
      for Candidate of Candidates loop
         declare
            DCO : constant Frequency_Type := AFE_Clk * Candidate;
            DCO_Centrality : constant Frequency_Type := abs (DCO - DCO_Mid);
         begin
            if DCO <= DCO_Max and DCO >= DCO_Min and
               DCO_Centrality < Best_DCO_Centrality
            then
               DCO_Found := True;
               Best_DCO_Centrality := DCO_Centrality;
               Best_Div := Candidate;
               Best_DCO := DCO;
            end if;
         end;
      end loop;

      if not DCO_Found then
         return;
      end if;

      if Best_Div mod 2 = 0 then
         pragma Assert (Div /= 1);
         pragma Assert (Div > 1);
         if Best_Div = 2 then
            PDiv := 2;
            QDiv := 1;
            KDiv := 1;
         elsif Best_Div mod 4 = 0 then
            PDiv := 2;
            QDiv := QDiv_Range(Best_Div / 4);
            KDiv := 2;
         elsif Best_Div mod 6 = 0 then
            PDiv := 3;
            QDiv := QDiv_Range(Best_Div / 6);
            KDiv := 2;
         elsif Best_Div mod 5 = 0 then
            PDiv := 5;
            QDiv := QDiv_Range(Best_Div / 10);
            KDiv := 2;
         elsif Best_Div mod 14 = 0 then
            PDiv := 7;
            QDiv := QDiv_Range(Best_Div / 14);
            KDiv := 2;
         end if;
      else
         if Best_Div = 3 or Best_Div = 5 or Best_Div = 7 then
            PDiv := QDiv_Range(Best_Div);
            QDiv := 1;
            KDiv := 1;
         else
            PDiv := QDiv_Range(Best_Div / 3);
            QDiv := 1;
            KDiv := 3;
         end if;
      end if;
      Params.KDiv := (case KDiv is
                      when      1 => 1,
                      when      2 => 2,
                      when      3 => 4,
                      when others => 1);
      Params.PDiv := (case PDiv is
                      when 2 => 1,
                      when 3 => 2,
                      when 5 => 4,
                      when 7 => 8,
                      when others => 1);
      Params.QDiv_Ratio := QDiv;
      Params.QDiv_Mode := (if QDiv = 1 then 0 else 1);
      Power_And_Clocks.Get_Refclk (Refclk_Freq);
      declare
         DCO_FRACTION_SHIFT : constant := 9;
         function Fraction_Part (DCO : Frequency_Type) return Word32 is
            (Shift_Left (Word32 ((DCO * 2 ** 15) / Refclk_Freq) and 16#7fff#,
               DCO_FRACTION_SHIFT));
      begin
         Params.DCO_Integer := Word32 (Best_DCO);
         Params.DCO_Fraction := Fraction_Part (Best_DCO);
      end;

      Success := True;
   end Calc_HDMI_PLL_Dividers;

   procedure On
     (PLL      : in     T;
      Port_Cfg : in     Port_Config;
      Success  :    out Boolean)
   is
      Params : PLL_Params;
   begin
      if Port_Cfg.Display = DP then
         Calc_DP_PLL_Dividers (PLL, Port_Cfg.DP.Bandwidth, Params, Success);
      else
         Calc_HDMI_PLL_Dividers (PLL, Port_Cfg.Mode.Dotclock, Params, Success);
      end if;

      if not Success then
         return;
      end if;

      Registers.Set_Mask
        (Register => DPLL_ENABLE (PLL),
         Mask     => DPLL_ENABLE_POWER_ENABLE);
      Registers.Wait_Set_Mask
        (Register => DPLL_ENABLE (PLL),
         Mask     => DPLL_ENABLE_POWER_STATE,
         Success  => Success);

      if not Success then
         return;
      end if;

      --Registers.Write
      --  (Register => DPLL_SSC (PLL),
      --   Value    => (if Port_Cfg.Display = DP then DPLL_SSC_DP else 0));

      Registers.Write
        (Register => DPLL_CFGCR0 (PLL),
         Value => Params.DCO_Fraction * 2 ** 10 or
                  Params.DCO_Integer);

      Registers.Write
        (Register => DPLL_CFGCR1 (PLL),
         Value => Word32(Params.QDiv_Ratio) * 2 ** 10 or
                  Word32(Params.QDiv_Mode) * 2 ** 9 or
                  Word32(Params.KDiv) * 2 ** 6 or
                  Word32(Params.PDiv) * 2 ** 2);
      Registers.Posting_Read(DPLL_CFGCR1 (PLL));

      Registers.Set_Mask
        (Register => DPLL_ENABLE (PLL),
         Mask     => DPLL_ENABLE_PLL_ENABLE);
      Registers.Wait_Set_Mask
        (Register => DPLL_ENABLE (PLL),
         Mask     => DPLL_ENABLE_PLL_LOCK,
         Success  => Success);
   end On;

   procedure Free (PLL : T)
   is
   begin
      Registers.Unset_Mask
        (Register => DPLL_ENABLE (PLL),
         Mask     => DPLL_ENABLE_PLL_ENABLE);
      Registers.Wait_Unset_Mask
        (Register => DPLL_ENABLE (PLL),
         Mask     => DPLL_ENABLE_PLL_LOCK);

      Registers.Unset_Mask
        (Register => DPLL_ENABLE (PLL),
         Mask     => DPLL_ENABLE_POWER_ENABLE);
      Registers.Wait_Unset_Mask
        (Register => DPLL_ENABLE (PLL),
         Mask     => DPLL_ENABLE_POWER_STATE);
   end Free;

   procedure All_Off is
   begin
      for PLL in Configurable_DPLLs loop
         Free (PLL);
      end loop;
   end All_Off;

   procedure Initialize is
   begin
      null;
   end Initialize;

end HW.GFX.GMA.PLLs.Combo_Phy;
