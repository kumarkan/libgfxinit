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

with HW.GFX.DP_Training;
with HW.GFX.GMA.Config;
with HW.GFX.GMA.DP_Aux_Ch;
with HW.GFX.GMA.DP_Info;
with HW.GFX.GMA.Transcoder;
with HW.GFX.GMA.Registers;

with HW.Debug;
with GNAT.Source_Info;

package body HW.GFX.GMA.Connectors.DDI is
   DDI_BUF_CTL_BUFFER_ENABLE        : constant :=  1 * 2 ** 31;
   DDI_BUF_CTL_PORT_WIDTH_MASK      : constant :=  7 * 2 **  1;
   DDI_BUF_CTL_PORT_WIDTH_1_LANE    : constant :=  0 * 2 **  1;
   DDI_BUF_CTL_PORT_WIDTH_2_LANES   : constant :=  1 * 2 **  1;
   DDI_BUF_CTL_PORT_WIDTH_4_LANES   : constant :=  3 * 2 **  1;
   DDI_BUF_CTL_PORT_REVERSAL        : constant :=  1 * 2 ** 16;
   DDI_BUF_CTL_IDLE_STATUS          : constant :=  1 * 2 **  7;

   type DDI_BUF_CTL_PORT_WIDTH_T is array (HW.GFX.DP_Lane_Count) of Word32;
   DDI_BUF_CTL_PORT_WIDTH : constant DDI_BUF_CTL_PORT_WIDTH_T :=
      DDI_BUF_CTL_PORT_WIDTH_T'
     (HW.GFX.DP_Lane_Count_1 => DDI_BUF_CTL_PORT_WIDTH_1_LANE,
      HW.GFX.DP_Lane_Count_2 => DDI_BUF_CTL_PORT_WIDTH_2_LANES,
      HW.GFX.DP_Lane_Count_4 => DDI_BUF_CTL_PORT_WIDTH_4_LANES);

   type Pipe_Regs is array (Pipe_Index) of Registers.Registers_Index;   
   DP_TP_CTL : constant Pipe_Regs := Pipe_Regs'
     (Primary   => Registers.DP_TP_CTL_A,
      Secondary => Registers.DP_TP_CTL_B,
      Tertiary  => Registers.DP_TP_CTL_C);
   DP_TP_CTL_TRANSPORT_ENABLE       : constant := 1 * 2 ** 31;
   DP_TP_CTL_MODE_SST               : constant := 0 * 2 ** 27;
   DP_TP_CTL_MODE_MST               : constant := 1 * 2 ** 27;
   DP_TP_CTL_FORCE_ACT              : constant := 1 * 2 ** 25;
   DP_TP_CTL_ENHANCED_FRAME_ENABLE  : constant := 1 * 2 ** 18;
   DP_TP_CTL_LINK_TRAIN_MASK        : constant := 7 * 2 **  8;
   DP_TP_CTL_LINK_TRAIN_PAT1        : constant := 0 * 2 **  8;
   DP_TP_CTL_LINK_TRAIN_PAT2        : constant := 1 * 2 **  8;
   DP_TP_CTL_LINK_TRAIN_PAT3        : constant := 4 * 2 **  8;
   DP_TP_CTL_LINK_TRAIN_IDLE        : constant := 2 * 2 **  8;
   DP_TP_CTL_LINK_TRAIN_NORMAL      : constant := 3 * 2 **  8;

   DP_TP_STATUS : constant Pipe_Regs := Pipe_Regs'
     (Primary   => Registers.TGL_DP_TP_STATUS_A,
      Secondary => Registers.TGL_DP_TP_STATUS_B,
      Tertiary  => Registers.TGL_DP_TP_STATUS_C);
   DP_TP_STATUS_MIN_IDLES_SENT      : constant := 1 * 2 ** 25;

   type Port_Regs_Record is record
      PORT_TX_DW5_LN0 : Registers.Registers_Index;
      PORT_TX_DW5_GRP : Registers.Registers_Index;
      PORT_TX_DW7_LN0 : Registers.Registers_Index;
      PORT_TX_DW7_GRP : Registers.Registers_Index;
      PORT_TX_DW2_LN0 : Registers.Registers_Index;
      PORT_TX_DW2_GRP : Registers.Registers_Index;
      PORT_TX_DW4_LN0 : Registers.Registers_Index;
      PORT_TX_DW4_LN1 : Registers.Registers_Index;
      PORT_TX_DW4_LN2 : Registers.Registers_Index;
      PORT_TX_DW4_LN3 : Registers.Registers_Index;
      PORT_TX_DW4_GRP : Registers.Registers_Index;
      PORT_PCS_DW1_LN0 : Registers.Registers_Index;
      PORT_PCS_DW1_GRP : Registers.Registers_Index;
      PORT_CL_DW5 : Registers.Registers_Index;
      PORT_CL_DW10 : Registers.Registers_Index;
      DDI_BUF_CTL : Registers.Registers_Index;
   end record;
   type Port_Regs_Array is array (Combo_Port) of Port_Regs_Record;
   Port_Regs : constant Port_Regs_Array :=
      Port_Regs_Array'
     (DIGI_A =>
       (Registers.PORT_TX_DW5_LN0_A, Registers.PORT_TX_DW5_GRP_A,
        Registers.PORT_TX_DW7_LN0_A, Registers.PORT_TX_DW7_GRP_A,
        Registers.PORT_TX_DW2_LN0_A, Registers.PORT_TX_DW2_GRP_A,
        Registers.PORT_TX_DW4_LN0_A, Registers.PORT_TX_DW4_LN1_A,
	   Registers.PORT_TX_DW4_LN2_A, Registers.PORT_TX_DW4_LN3_A,
	   Registers.PORT_TX_DW4_GRP_A,
        Registers.PORT_PCS_DW1_LN0_A, Registers.PORT_PCS_DW1_GRP_A,
	Registers.PORT_CL_DW5_A,
	Registers.PORT_CL_DW10_A,
        Registers.DDI_BUF_CTL_A),
      DIGI_B =>
        (Registers.PORT_TX_DW5_LN0_B, Registers.PORT_TX_DW5_GRP_B,
         Registers.PORT_TX_DW7_LN0_B, Registers.PORT_TX_DW7_GRP_B,
         Registers.PORT_TX_DW2_LN0_B, Registers.PORT_TX_DW2_GRP_B,
         Registers.PORT_TX_DW4_LN0_B, Registers.PORT_TX_DW4_LN1_B,
	    Registers.PORT_TX_DW4_LN2_B, Registers.PORT_TX_DW4_LN3_B,
	    Registers.PORT_TX_DW4_GRP_B,
         Registers.PORT_PCS_DW1_LN0_B, Registers.PORT_PCS_DW1_GRP_B,
         Registers.PORT_CL_DW5_B,
         Registers.PORT_CL_DW10_B,
         Registers.DDI_BUF_CTL_B),
      DIGI_C =>
        (Registers.PORT_TX_DW5_LN0_C, Registers.PORT_TX_DW5_GRP_C,
         Registers.PORT_TX_DW7_LN0_C, Registers.PORT_TX_DW7_GRP_C,
         Registers.PORT_TX_DW2_LN0_C, Registers.PORT_TX_DW2_GRP_C,
         Registers.PORT_TX_DW4_LN0_C, Registers.PORT_TX_DW4_LN1_C,
	    Registers.PORT_TX_DW4_LN2_C, Registers.PORT_TX_DW4_LN3_C,
	    Registers.PORT_TX_DW4_GRP_C,
         Registers.PORT_PCS_DW1_LN0_C, Registers.PORT_PCS_DW1_GRP_C,
         Registers.PORT_CL_DW5_C,
         Registers.PORT_CL_DW10_C,
         Registers.DDI_BUF_CTL_C));

   type Lanes is (LN0, LN1, LN2, LN3);
   function PORT_TX_DW4 (Lane : Lanes; Port : Combo_Port)
      return Registers.Registers_Index
   is (case Lane is
       when LN0 => Port_Regs (Port).PORT_TX_DW4_LN0,
       when LN1 => Port_Regs (Port).PORT_TX_DW4_LN1,
       when LN2 => Port_Regs (Port).PORT_TX_DW4_LN2,
       when LN3 => Port_Regs (Port).PORT_TX_DW4_LN3);

   PORT_PCS_DW1_LN0_COMMON_KEEPER : constant := 1 * 2 ** 26;

   type Post_Cursor is new Natural range 0 .. 63;
   function PORT_TX_DW4_POST_CURSOR1 (P : Post_Cursor) return Word32 is
      (Shift_Left (Word32 (P), 12));
   function PORT_TX_DW4_POST_CURSOR2 (P : Post_Cursor) return Word32 is
      (Shift_Left (Word32 (P), 6));

   type Cursor_Coeff is new Natural range 0 .. 63;
   function PORT_TX_DW4_CURSOR_COEFF (C : Cursor_Coeff) return Word32 is
      (Word32 (C));

   type N_Scalar is new Natural range 0 .. 127;
   function PORT_TX_DW7_N_SCALAR (N : N_Scalar) return Word32 is
      (Shift_Left (Word32 (N), 24));

   type Swing_Select is new Natural range 0 .. 15;
   function PORT_TX_SWING_SEL_UPPER (S : Swing_Select) return Word32 is
      (Shift_Left (Shift_Right (Word32(S), 3), 15));
   function PORT_TX_SWING_SEL_LOWER (S : Swing_Select) return Word32 is
      (Shift_Left (Word32(S), 11));

   type Buffer_Trans is record
     DW2_SWING_SEL    : Swing_Select;
     DW7_N_SCALAR     : N_Scalar;
     DW4_CURSOR_COEFF : Cursor_Coeff;
     DW4_POST_CURSOR2 : Post_Cursor;
     DW4_POST_CURSOR1 : Post_Cursor;
   end record;

   type Buffer_Trans_Range is new Natural range 0 .. 9;
   type Buffer_Trans_Array is array (Buffer_Trans_Range) of Buffer_Trans;

   -- _tgl_combo_phy_trans_dp_hbr
   Buffer_Trans_DP_HBR : constant Buffer_Trans_Array :=
      Buffer_Trans_Array'(
     (16#a#, 16#32#, 16#3f#, 16#00#, 16#00#),
     (16#a#, 16#4a#, 16#37#, 16#00#, 16#00#),
     (16#c#, 16#71#, 16#2f#, 16#00#, 16#10#),
     (16#6#, 16#7d#, 16#2b#, 16#00#, 16#14#),
     (16#a#, 16#4c#, 16#3f#, 16#00#, 16#00#),
     (16#c#, 16#73#, 16#34#, 16#00#, 16#0b#),
     (16#6#, 16#7f#, 16#2f#, 16#00#, 16#10#),
     (16#c#, 16#6c#, 16#3c#, 16#00#, 16#03#),
     (16#6#, 16#7f#, 16#35#, 16#00#, 16#0a#),
     (16#6#, 16#7f#, 16#3f#, 16#00#, 16#00#));

   -- _tgl_combo_phy_trans_dp_hbr2
   Buffer_Trans_DP_HBR2 : constant Buffer_Trans_Array :=
      Buffer_Trans_Array'(
     (16#a#, 16#35#, 16#3f#, 16#00#, 16#00#),
     (16#a#, 16#4f#, 16#37#, 16#00#, 16#08#),
     (16#c#, 16#63#, 16#2f#, 16#00#, 16#10#),
     (16#6#, 16#7f#, 16#2b#, 16#00#, 16#14#),
     (16#a#, 16#47#, 16#3f#, 16#00#, 16#00#),
     (16#c#, 16#63#, 16#34#, 16#00#, 16#0b#),
     (16#6#, 16#7f#, 16#2f#, 16#00#, 16#10#),
     (16#c#, 16#61#, 16#3c#, 16#00#, 16#03#),
     (16#6#, 16#7b#, 16#35#, 16#00#, 16#0a#),
     (16#6#, 16#7f#, 16#3f#, 16#00#, 16#00#));

   -- _tgl_uy_combo_phy_trans_dp_hbr2
   Buffer_Trans_DP_HBR2_U_Y : constant Buffer_Trans_Array :=
      Buffer_Trans_Array'(
     (16#a#, 16#35#, 16#3f#, 16#00#, 16#00#),
     (16#a#, 16#4f#, 16#36#, 16#00#, 16#09#),
     (16#c#, 16#60#, 16#32#, 16#00#, 16#0d#),
     (16#c#, 16#7f#, 16#2d#, 16#00#, 16#12#),
     (16#c#, 16#47#, 16#3f#, 16#00#, 16#00#),
     (16#c#, 16#6f#, 16#36#, 16#00#, 16#09#),
     (16#6#, 16#7d#, 16#32#, 16#00#, 16#0d#),
     (16#6#, 16#60#, 16#3c#, 16#00#, 16#03#),
     (16#6#, 16#7f#, 16#34#, 16#00#, 16#0b#),
     (16#6#, 16#7f#, 16#3f#, 16#00#, 16#00#));

   --_icl_combo_phy_trans_dp_hbr2_edp_hbr3
   Buffer_Trans_DP_HBR2_EDP_HBR3 : constant Buffer_Trans_Array :=
      Buffer_Trans_Array'(
     (16#a#, 16#35#, 16#3f#, 16#00#, 16#00#),
     (16#a#, 16#4f#, 16#37#, 16#00#, 16#08#),
     (16#c#, 16#71#, 16#2f#, 16#00#, 16#10#),
     (16#6#, 16#7f#, 16#2b#, 16#00#, 16#14#),
     (16#a#, 16#4c#, 16#3f#, 16#00#, 16#00#),
     (16#c#, 16#73#, 16#34#, 16#00#, 16#0b#),
     (16#6#, 16#7f#, 16#2f#, 16#00#, 16#10#),
     (16#c#, 16#6c#, 16#3c#, 16#00#, 16#03#),
     (16#6#, 16#7f#, 16#35#, 16#00#, 16#0a#),
     (16#6#, 16#7f#, 16#3f#, 16#00#, 16#00#));

   -- _icl_combo_phy_trans_edp_hbr2
   Buffer_Trans_EDP_HBR2 : constant Buffer_Trans_Array :=
      Buffer_Trans_Array'(
     (16#0#, 16#7F#, 16#3F#, 16#00#, 16#00#),
     (16#8#, 16#7F#, 16#38#, 16#00#, 16#07#),
     (16#1#, 16#7F#, 16#33#, 16#00#, 16#0C#),
     (16#9#, 16#7F#, 16#31#, 16#00#, 16#0E#),
     (16#8#, 16#7F#, 16#3F#, 16#00#, 16#00#),
     (16#1#, 16#7F#, 16#38#, 16#00#, 16#07#),
     (16#9#, 16#7F#, 16#35#, 16#00#, 16#0A#),
     (16#1#, 16#7F#, 16#3F#, 16#00#, 16#00#),
     (16#9#, 16#7F#, 16#38#, 16#00#, 16#07#),
     (16#9#, 16#7F#, 16#3F#, 16#00#, 16#00#));

   PORT_CL_DW10_PWR_UP_ALL        : constant :=     0 * 2 ** 4;
   PORT_CL_DW10_PWR_DOWN_LN_3_2   : constant := 16#c# * 2 ** 4;
   PORT_CL_DW10_PWR_DOWN_LN_3_2_1 : constant := 16#e# * 2 ** 4;
   PORT_CL_DW10_PWR_DOWN_LN_1_0   : constant := 16#3# * 2 ** 4;
   PORT_CL_DW10_PWR_DOWN_LN_2_1_0 : constant := 16#7# * 2 ** 4;

   type DDI_CLK_SEL_Array is array (Combo_Port) of Natural;
   DDI_CLK_SEL_SHIFT : constant DDI_CLK_SEL_Array :=
      DDI_CLK_SEL_Array'
     (DIGI_A => 0,
      DIGI_B => 2,
      DIGI_C => 4);

   type TGL_Digital_Port_Array is array (TGL_Digital_Port) of Word32;
   DDI_CLK_OFF : constant TGL_Digital_Port_Array :=
      TGL_Digital_Port_Array'
     (DIGI_A  => 1 * 2 ** 10,
      DIGI_B  => 1 * 2 ** 11,
      DIGI_C  => 1 * 2 ** 24,
      DDI_TC1 => 1 * 2 ** 12,
      DDI_TC2 => 1 * 2 ** 13,
      DDI_TC3 => 1 * 2 ** 14,
      DDI_TC4 => 1 * 2 ** 21,
      DDI_TC5 => 1 * 2 ** 22,
      DDI_TC6 => 1 * 2 ** 23,
      others  => 0);

   procedure Initialize is
   begin
      pragma Debug (Debug.Put_Line (GNAT.Source_Info.Enclosing_Entity));
   end Initialize;

   procedure Set_Vswing_And_Deemphasis
     (Pipe      : Pipe_Index;
      Port      : TGL_Digital_Port;
      Link      : DP_Link;
      Train_Set : DP_Info.Train_Set)
   is
      type Training_Values is (Training_Enable, Training_Disable);

      PORT_TX_DW5_TX_TRAINING_EN        : constant := 1 * 2 ** 31;
      PORT_TX_DW5_SCALING_MODE_SEL_MASK : constant := 7 * 2 ** 18;
      PORT_TX_DW5_RTERM_SELECT_MASK     : constant := 7 * 2 ** 3;
      PORT_TX_DW5_TAP2_DISABLE          : constant := 1 * 2 ** 30;
      PORT_TX_DW5_TAP3_DISABLE          : constant := 1 * 2 ** 29;

      PORT_TX_DW2_RCOMP_SCALAR_MASK     : constant := 16#ff# * 2 ** 0;
      PORT_TX_DW2_SWING_SEL_LOWER_MASK  : constant :=      7 * 2 ** 11;
      PORT_TX_DW2_SWING_SEL_UPPER       : constant :=      1 * 2 ** 15;

      PORT_TX_DW4_LOADGEN_SELECT        : constant :=      1 * 2 ** 31;
      PORT_TX_DW4_POST_CURSOR2_MASK     : constant := 16#3f# * 2 ** 6;
      PORT_TX_DW4_POST_CURSOR1_MASK     : constant := 16#3f# * 2 ** 12;
      PORT_TX_DW4_CURSOR_COEFF_MASK     : constant := 16#3f# * 2 ** 0;

      PORT_TX_DW7_N_SCALAR_MASK         : constant := 16#7f# * 2 ** 24;

      type Scaling_Mode is new Natural range 0 .. 7;
      function PORT_TX_DW5_SCALING_MODE_SEL (S : Scaling_Mode) return Word32 is
         (Shift_Left (Word32 (S), 18));

      type Rterm_Select is new Natural range 0 .. 7;
      function PORT_TX_DW5_RTERM_SELECT (R : Rterm_Select) return Word32 is
         (Shift_Left (Word32 (R), 3));

      type Rcomp_Scalar is new Natural range 0 .. 255;
      function PORT_TX_DW2_RCOMP_SCALAR (R : Rcomp_Scalar) return Word32 is
         (Word32 (R));

      procedure Set_Tx_Training (Port : GPU_Port; Training : Training_Values) is
         DW5 : Word32;
      begin
        Registers.Read (Port_Regs (Port).PORT_TX_DW5_LN0, DW5);
        Registers.Write
          (Register => Port_Regs (Port).PORT_TX_DW5_GRP,
           Value    => (if Training = Training_Enable
                        then DW5 or PORT_TX_DW5_TX_TRAINING_EN
                        else DW5 and not PORT_TX_DW5_TX_TRAINING_EN));
      end Set_Tx_Training;

      Tmp : Word32;
      Entry_Index : Buffer_Trans_Range;
      Buf_Trans : Buffer_Trans_Array;
      PORT_CL_DW5_SUS_CLOCK_CONFIG : constant := 3 * 2 ** 0;
   begin
      case Train_Set.Voltage_Swing is
         when DP_Info.VS_Level_0 =>
            case Train_Set.Pre_Emph is
               when DP_Info.Emph_Level_0 => Entry_Index := 0;
               when DP_Info.Emph_Level_1 => Entry_Index := 1;
               when DP_Info.Emph_Level_2 => Entry_Index := 2;
               when DP_Info.Emph_Level_3 => Entry_Index := 3;
            end case;
         when DP_Info.VS_Level_1 =>
            case Train_Set.Pre_Emph is
               when DP_Info.Emph_Level_0 => Entry_Index := 4;
               when DP_Info.Emph_Level_1 => Entry_Index := 5;
               when DP_Info.Emph_Level_2 => Entry_Index := 6;
               when others =>               Entry_Index := 0;
            end case;
         when DP_Info.VS_Level_2 =>
            case Train_Set.Pre_Emph is
               when DP_Info.Emph_Level_0 => Entry_Index := 7;
               when DP_Info.Emph_Level_1 => Entry_Index := 8;
               when others =>               Entry_Index := 0;
            end case;
         when DP_Info.VS_Level_3 =>
            case Train_Set.Pre_Emph is
               when DP_Info.Emph_Level_0 => Entry_Index := 9;
               when others =>               Entry_Index := 0;
            end case;
      end case;

      Buf_Trans := Buffer_Trans_DP_HBR2;
      Debug.Put_Reg32 ("Selecting buffer translations index ", Word32(Entry_Index));

      --if Port = eDP then
         --if Link.Bandwidth > DP_Bandwidth_5_4 then
            -- icl_combo_phy_trans_dp_hbr2_edp_hbr3
         --   Buf_Trans := Buffer_Trans_DP_HBR2_EDP_HBR3;
         --elseif VBT requests "HOBL" and hobl isn't failed
         --   then use HOBL table (tgl_combo_phy_trans_edp_hbr2_hobl)
         --else if VBT requests "low vswing" then
         --    Buf_Trans := Buffer_Trans_EDP_HBR2;
	 --else
	 --    
         --end if;
      --else
         --if Link.Bandwidth > DP_Bandwidth_2_7 then
         --   if Config.Is_LP then
         --      Buf_Trans := Buffer_Trans_DP_HBR2_U_Y;
         --   else
         --      Buf_Trans := Buffer_Trans_DP_HBR2;
         --   end if;
         --else
         --   Buf_Trans := Buffer_Trans_DP_HBR;
         --end if;
      --end if;

      -- Enable common keeper for DP/eDP
      -- if Port_Cfg.Display = DP then
      Registers.Read (Port_Regs (Port).PORT_PCS_DW1_LN0, Tmp);
      Tmp := Tmp or PORT_PCS_DW1_LN0_COMMON_KEEPER;
      Registers.Write (Port_Regs (Port).PORT_PCS_DW1_GRP, Tmp);

      -- Program loadgen select (group access not allowed since each lane may
      -- have a unique value
      -- <= 6GHz, 4 lanes   (0, 1, 1, 1)
      -- <= 6GHz, 1,2 lanes (0, 1, 1, 0)
      -- > 6GHz,  any lanes (0, 0, 0, 0)
      for Lane in Lanes loop
         declare
            Loadgen : constant Word32 :=
              (if (Link.Bandwidth <= DP_Bandwidth_5_4 and
                     Link.Lane_Count = DP_Lane_Count_4 and
                     Lane /= LN1) or
                  (Link.Bandwidth <= DP_Bandwidth_5_4 and
                     Link.Lane_Count /= DP_Lane_Count_4 and
                     (Lane = LN1 or Lane = LN2))
               then PORT_TX_DW4_LOADGEN_SELECT
               else 0);
         begin
            Registers.Unset_And_Set_Mask
              (Register   => PORT_TX_DW4 (Lane, Port),
               Mask_Unset => PORT_TX_DW4_LOADGEN_SELECT,
               Mask_Set   => Loadgen);
         end;
      end loop;

      Registers.Set_Mask
        (Register => Port_Regs (Port).PORT_CL_DW5,
         Mask     => PORT_CL_DW5_SUS_CLOCK_CONFIG);

      Set_Tx_Training (Port, Training_Disable);

      -- program swing and deemphasis

      -- clear EDP4K2K_MODE_OVRD_EN | EDP4K2K_MODE_OVRD_OPTIMIZED if not HOBL
      --Registers.Unset_Mask
      -- (Register => Port_Regs (Port).PORT_CL_DW10,
      --  Mask     => 3 * 2 ** 2);

      Registers.Read (Port_Regs (Port).PORT_TX_DW5_LN0, Tmp);
      Tmp := Tmp and not
         (PORT_TX_DW5_SCALING_MODE_SEL_MASK or
          PORT_TX_DW5_RTERM_SELECT_MASK or
          PORT_TX_DW5_TAP2_DISABLE or
          PORT_TX_DW5_TAP3_DISABLE);
      Tmp := Tmp or
         PORT_TX_DW5_SCALING_MODE_SEL (2) or
         PORT_TX_DW5_RTERM_SELECT (6) or
         PORT_TX_DW5_TAP3_DISABLE;
      Registers.Write (Port_Regs (Port).PORT_TX_DW5_GRP, Tmp);

      Registers.Read (Port_Regs (Port).PORT_TX_DW2_LN0, Tmp);
      Tmp := Tmp and not
        (PORT_TX_DW2_RCOMP_SCALAR_MASK or
         PORT_TX_DW2_SWING_SEL_LOWER_MASK or
         PORT_TX_DW2_SWING_SEL_UPPER);
      Tmp := Tmp or PORT_TX_SWING_SEL_UPPER (Buf_Trans (Entry_Index).DW2_SWING_SEL);
      Tmp := Tmp or PORT_TX_SWING_SEL_LOWER (Buf_Trans (Entry_Index).DW2_SWING_SEL);
      Tmp := Tmp or PORT_TX_DW2_RCOMP_SCALAR (16#98#);
      Registers.Write (Port_Regs (Port).PORT_TX_DW2_GRP, Tmp);

      -- PORT_TX_DW4
      -- Cannot write to GRP, b/c it would overwrite individual loadgen bits
      for Lane in Lanes loop
         Registers.Read (PORT_TX_DW4 (Lane, Port), Tmp);
         Tmp := Tmp and not
            (PORT_TX_DW4_POST_CURSOR2_MASK or
             PORT_TX_DW4_POST_CURSOR1_MASK or
             PORT_TX_DW4_CURSOR_COEFF_MASK);
         Tmp := Tmp or PORT_TX_DW4_POST_CURSOR1 (Buf_Trans (Entry_Index).DW4_POST_CURSOR1);
         Tmp := Tmp or PORT_TX_DW4_POST_CURSOR2 (Buf_Trans (Entry_Index).DW4_POST_CURSOR2);
         Tmp := Tmp or PORT_TX_DW4_CURSOR_COEFF (Buf_Trans (Entry_Index).DW4_CURSOR_COEFF);
         Registers.Write (PORT_TX_DW4 (Lane, Port), Tmp);
      end loop;

      -- PORT_TX_DW7
      Registers.Read (Port_Regs (Port).PORT_TX_DW7_LN0, Tmp);
      Tmp := Tmp and not PORT_TX_DW7_N_SCALAR_MASK;
      Tmp := Tmp or PORT_TX_DW7_N_SCALAR (Buf_Trans (Entry_Index).DW7_N_SCALAR);
      Registers.Write (Port_Regs (Port).PORT_TX_DW7_GRP, Tmp);

      Set_Tx_Training (Port, Training_Enable);
   end Set_Vswing_And_Deemphasis;

   procedure Set_TP_CTL
     (Pipe : Pipe_Index;
      Port : TGL_Digital_Port;
      Link : DP_Link;
      Pattern : DP_Info.Training_Pattern)
   is
      type DP_TP_CTL_LINK_TRAIN_ARRAY is
         array (DP_Info.Training_Pattern) of Word32;
      DP_TP_CTL_LINK_TRAIN : constant DP_TP_CTL_LINK_TRAIN_ARRAY :=
         DP_TP_CTL_LINK_TRAIN_ARRAY'
        (DP_Info.TP_1    => DP_TP_CTL_LINK_TRAIN_PAT1,
         DP_Info.TP_2    => DP_TP_CTL_LINK_TRAIN_PAT2,
         DP_Info.TP_3    => DP_TP_CTL_LINK_TRAIN_PAT3,
         DP_Info.TP_Idle => DP_TP_CTL_LINK_TRAIN_IDLE,
         DP_Info.TP_None => DP_TP_CTL_LINK_TRAIN_NORMAL);

      DP_TP_CTL_Enhanced_Frame : Word32 := 0;
   begin
      if Link.Enhanced_Framing then
         DP_TP_CTL_Enhanced_Frame := DP_TP_CTL_ENHANCED_FRAME_ENABLE;
      end if;

      Registers.Write
        (Register => DP_TP_CTL (Pipe),
         Value    => DP_TP_CTL_TRANSPORT_ENABLE or
                     DP_TP_CTL_Enhanced_Frame or
                     DP_TP_CTL_LINK_TRAIN (Pattern));
   end Set_TP_CTL;

   procedure Set_Training_Pattern
     (Pipe    : Pipe_Index;
      Port    : TGL_Digital_Port;
      Link    : DP_Link;
      Pattern : DP_Info.Training_Pattern)
   is
      use type DP_Info.Training_Pattern;
   begin
      Debug.Put ("Setting training pattern to ");
      case Pattern is
         when DP_Info.TP_Idle => Debug.Put_Line ("Idle");
	 when DP_Info.TP_1 => Debug.Put_Line ("TP 1");
	 when DP_Info.TP_2 => Debug.Put_Line ("TP 2");
	 when DP_Info.TP_3 => Debug.Put_Line ("TP 3");
	 when DP_Info.TP_None => Debug.Put_Line ("None");
      end case;
      if Pattern < DP_Info.TP_Idle then
         Set_TP_CTL (Pipe, Port, Link, Pattern);
      else
         -- Send at least 5 idle patterns
         Set_TP_CTL (Pipe, Port, Link, DP_Info.TP_Idle);

         Registers.Wait_Set_Mask
           (Register => DP_TP_STATUS (Pipe),
            Mask => DP_TP_STATUS_MIN_IDLES_SENT);
         Set_TP_CTL (Pipe, Port, Link, DP_Info.TP_None);
      end if;
   end Set_Training_Pattern;

   procedure Set_Signal_Levels
     (Pipe        : Pipe_Index;
      Port        : TGL_Digital_Port;
      Link        : DP_Link;
      Train_Set   : DP_Info.Train_Set)
   is
      Was_Enabled : Boolean;
   begin
      pragma Debug (Debug.Put_Line (GNAT.Source_Info.Enclosing_Entity));

      Registers.Is_Set_Mask
        (Register => Port_Regs (Port).DDI_BUF_CTL,
         Mask     => DDI_BUF_CTL_BUFFER_ENABLE,
         Result   => Was_Enabled);

      Set_Vswing_And_Deemphasis (Pipe, Port, Link, Train_Set);

      if not Was_Enabled and Port in Combo_Port then
         declare
            Lane_Mask : constant Word32 :=
              (case Link.Lane_Count is
               -- if Lane_Reversal then PWR_DOWN_LN_2_1_0 else PORT_CL_DW10_PWR_DOWN_LN_3_2_1
               when DP_Lane_Count_1 => PORT_CL_DW10_PWR_DOWN_LN_3_2_1,
               -- if Lane_Reversal then PWR_DOWN_1_0 else PORT_CL_DW10_PWR_DOWN_LN_3_2
               when DP_Lane_Count_2 => PORT_CL_DW10_PWR_DOWN_LN_3_2,
               when others => PORT_CL_DW10_PWR_UP_ALL);
            PORT_CL_DW10_PWR_DOWN_LN_MASK : constant := 16#f# * 2 ** 4;
         begin
            Registers.Unset_And_Set_Mask
              (Register   => Port_Regs (Port).PORT_CL_DW10,
               Mask_Unset => PORT_CL_DW10_PWR_DOWN_LN_MASK,
               Mask_Set   => Lane_Mask);
         end;
      end if;

      -- enable DDI buffer
      Registers.Unset_And_Set_Mask
        (Register    => Port_Regs (Port).DDI_BUF_CTL,
         Mask_Unset  => 16#f# * 2 ** 24 or -- DDI_BUF_CTL_TRANS_SELECT_MASK or
                        DDI_BUF_CTL_PORT_REVERSAL or
                        DDI_BUF_CTL_PORT_WIDTH_MASK,
         Mask_Set    => DDI_BUF_CTL_BUFFER_ENABLE or
                        DDI_BUF_CTL_PORT_WIDTH (Link.Lane_Count));
      Registers.Posting_Read (Port_Regs (Port).DDI_BUF_CTL);

      if not Was_Enabled then
         Registers.Wait_Unset_Mask (Port_Regs (Port).DDI_BUF_CTL, DDI_BUF_CTL_IDLE_STATUS);
      end if;
   end Set_Signal_Levels;

   pragma Warnings (GNATprove, Off, "unused variable ""Port""",
                    Reason => "Needed for a common interface");
   function Max_V_Swing
     (Port : TGL_Digital_Port) return DP_Info.DP_Voltage_Swing
   is
   begin
      return DP_Info.VS_Level_3;
   end Max_V_Swing;
   
   function Max_Pre_Emph
     (Port        : TGL_Digital_Port;
      Train_Set   : DP_Info.Train_Set)
      return DP_Info.DP_Pre_Emph
   is
   begin
      return
        (case Train_Set.Voltage_Swing is
         when DP_Info.VS_Level_0 => DP_Info.Emph_Level_3,
         when DP_Info.VS_Level_1 => DP_Info.Emph_Level_2,
         when DP_Info.VS_Level_2 => DP_Info.Emph_Level_1,
         when others             => DP_Info.Emph_Level_0);
   end Max_Pre_Emph;
   pragma Warnings (GNATprove, On, "unused variable ""Port""");

   procedure Pre_On
     (Pipe     : in     Pipe_Index;
      Port_Cfg : in     Port_Config;
      PLL_Hint : in     Word32;
      Success  :    out Boolean)
   is
      function To_DP (Port : TGL_Digital_Port) return DP_Port is
        (case Port is
         when DIGI_A  => DP_A,
         when DIGI_B  => DP_B,
         when DIGI_C  => DP_C,
         when DDI_TC1 => DP_D,
         when DDI_TC2 => DP_E,
         when DDI_TC3 => DP_F,
         when DDI_TC4 => DP_G,
         when DDI_TC5 => DP_H,
         when DDI_TC6 => DP_I,
         when others  => DP_A);
      package Training is new DP_Training
        (TPS3_Supported    => True,
         T                 => TGL_Digital_Port,
         Aux_T             => DP_Port,
         Aux_Ch            => DP_Aux_Ch,
         DP_Info           => DP_Info,
         To_Aux            => To_DP,
         Max_V_Swing       => Max_V_Swing,
         Max_Pre_Emph      => Max_Pre_Emph,
         Set_Pattern       => Set_Training_Pattern,
         Set_Signal_Levels => Set_Signal_Levels,
         Off               => Off);
   begin
      -- NOTE: THIS IS ONLY THE DISPLAYPORT SEQUENCE
      -- HDMI IS DIFFERENT

      -- Map the selected PLL to this port
      Registers.Unset_And_Set_Mask
        (Register   => Registers.DPCLKA_CFGCR0,
         Mask_Unset => 3 * 2 ** DDI_CLK_SEL_SHIFT (Port_Cfg.Port),
         Mask_Set   =>
            Shift_Left (PLL_Hint, DDI_CLK_SEL_SHIFT (Port_Cfg.Port)));

     -- This must be a separate register write from the prior step.
     Registers.Unset_Mask
       (Register => Registers.DPCLKA_CFGCR0,
        Mask     => DDI_CLK_OFF (Port_Cfg.Port));

      -- 3) program DP_MODE (not applicable to combo phy or TC in TBT alt mode)
      --- TODO 

      Transcoder.Enable_Pipe_Clock (Pipe, Port_Cfg);
      Transcoder.Configure (Pipe, Port_Cfg, False);

      -- configure DDI buffer
      Registers.Unset_And_Set_Mask
        (Register    => Port_Regs (Port_Cfg.Port).DDI_BUF_CTL,
         Mask_Unset  => 16#f# * 2 ** 24 or -- DDI_BUF_CTL_TRANS_SELECT_MASK or
                        DDI_BUF_CTL_PORT_REVERSAL or
                        DDI_BUF_CTL_PORT_WIDTH_MASK or
			DDI_BUF_CTL_BUFFER_ENABLE,
         Mask_Set    => DDI_BUF_CTL_PORT_WIDTH (Port_Cfg.DP.Lane_Count) or
	                DDI_BUF_CTL_BUFFER_ENABLE);
      Registers.Posting_Read (Port_Regs (Port_Cfg.Port).DDI_BUF_CTL);

      Training.Train_DP
        (Pipe => Pipe,
         Port => Port_Cfg.Port,
         Link => Port_Cfg.DP,
         Success => Success);
   end Pre_On;

   procedure Off (Pipe : Pipe_Index; Port : TGL_Digital_Port)
   is
      Enabled : Boolean;
   begin
      pragma Debug (Debug.Put_Line (GNAT.Source_Info.Enclosing_Entity));
      Registers.Is_Set_Mask
        (Register => Port_Regs (Port).DDI_BUF_CTL,
         Mask     => DDI_BUF_CTL_BUFFER_ENABLE,
         Result   => Enabled);

      if Enabled then
         Registers.Unset_Mask
           (Register => Port_Regs (Port).DDI_BUF_CTL,
            Mask     => DDI_BUF_CTL_BUFFER_ENABLE);
      end if;

      Registers.Unset_And_Set_Mask
        (Register   => DP_TP_CTL (Pipe),
         Mask_Unset => DP_TP_CTL_TRANSPORT_ENABLE or
                       DP_TP_CTL_LINK_TRAIN_MASK,
	 Mask_Set   => DP_TP_CTL_LINK_TRAIN_IDLE);

      if Enabled then
         Registers.Wait_Set_Mask
           (Register => Port_Regs (Port).DDI_BUF_CTL,
            Mask     => DDI_BUF_CTL_IDLE_STATUS);
      end if;

     Registers.Set_Mask
       (Register => Registers.DPCLKA_CFGCR0,
        Mask     => DDI_CLK_OFF (Port));
   end Off;
end HW.GFX.GMA.Connectors.DDI;
