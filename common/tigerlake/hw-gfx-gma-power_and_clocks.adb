-- Copyright 2021 Tim Wawrzynczak

-- GNU blah blah

with GNAT.Source_Info;

with HW.Time;
with HW.Debug;
with HW.GFX.GMA.Config;
with HW.GFX.GMA.Registers;
with HW.GFX.GMA.PCode;
with HW.GFX.GMA.Transcoder;
with HW.GFX.GMA.Combo_Phy;

use type HW.Word64;

package body HW.GFX.GMA.Power_And_Clocks is
   
   type Power_Domain is (PG0, PG1, PG2, PG3, PG4, PG5);
   subtype Power_Well is Power_Domain;
   subtype Dynamic_Domain is Power_Domain range PG1 .. PG5;
   
   constant CDCLK_CTL_CD_FREQ_SELECT_168_MHZ : 2#00101001110#;
   constant CDCLK_CTL_CD_FREQ_SELECT_172_MHZ : 2#00101011000#;
   constant CDCLK_CTL_CD_FREQ_SELECT_179_MHZ : 2#00101100100#;
   constant CDCLK_CTL_CD_FREQ_SELECT_180_MHZ : 2#00101100110#;
   constant CDCLK_CTL_CD_FREQ_SELECT_192_MHZ : 2#00101111110#;
   constant CDCLK_CTL_CD_FREQ_SELECT_307_MHZ : 2#01001100100#;
   constant CDCLK_CTL_CD_FREQ_SELECT_312_MHZ : 2#01001101110#;
   constant CDCLK_CTL_CD_FREQ_SELECT_324_MHZ : 2#01010000110#;
   constant CDCLK_CTL_CD_FREQ_SELECT_326_MHZ : 2#01010001011#;
   constant CDCLK_CTL_CD_FREQ_SELECT_480_MHZ : 2#01110111110#;
   constant CDCLK_CTL_CD_FREQ_SELECT_552_MHZ : 2#10001001110#;
   constant CDCLK_CTL_CD_FREQ_SELECT_556_MHZ : 2#10001011000#;
   constant CDCLK_CTL_CD_FREQ_SELECT_648_MHZ : 2#10100001110#;
   constant CDCLK_CTL_CD_FREQ_SELECT_652_MHZ : 2#10100011000#;
   
   procedure PD_On (PD : Power_Domain)
   is
      Ctl1, Ctl2, Ctl3, Ctl : Word32;
   begin
      pragma Debug (Debug.Put_Line (GNAT.Source_Info.Enclosing_Entity));
      
      Registers.Read (Registers.PWR_WELL_CTL_BIOS, Ctl1);
      Registers.Read (Registers.PWR_WELL_CTL_DRIVER, Ctl2);
      Registers.Read (Registers.PWR_WELL_CTL_KVMR, Ctl3);
      Registers.Read (Registers.PWR_WELL_CTL_DEBUG, Ctl4);
      
      if ((Ctl1 or Ctl2 or Ctl3 or Ctl4) and
	    PWR_WELL_CTL_POWER_REQUEST (PD)) = 0
      then
	 Registers.Wait_Unset_Mask
	   (Register => Registers.PWR_WELL_CTL_DRIVER,
	    Mask     => PWR_WELL_CTL_POWER_STATE (PD));
      end if;
      
      --   Set PWR_WELL_CTL Power Well 1 Request to 1b
      --      (BIOS uses PWR_WELL_CTL1, driver uses PWR_WELL_CTL2)      
      if (Ctl1 and PWR_WELL_CTL_POWER_REQUEST (PD)) = 0 then
	 Registers.Set_Mask
	   (Register => Registers.PWR_WELL_CTL_BIOS,
	    Mask     => PWR_WELL_CTL_POWER_REQUEST (PD));
	 
	 --   Poll PWR_WELL_CTL Power Well 1 State = 1b (45us)
	 Registers.Wait_Set_Mask
	   (Register => Registers.PWR_WELL_CTL_BIOS,
	    Mask     => PWR_WELL_CTL_POWER_REQUEST (PD));
	 
	 --   Poll FUSE_STATUS Fuse PG1 Distribution Status = 1b (20us)
	 if PD in Power_Well then
	    Registers.Wait_Set_Mask
	      (Register => Registers.FUSE_STATUS,
	       Mask     => FUSE_STATUS_PGx_DIST_STATUS (PD));
	 end if;
      end if;
   end PD_On;
   
   
   
   procedure Set_CDClk (CDClk_In : Frequency_Type)
     procedure Get_Cdclk (Refclk : Frequency_Type;
			  Cdclk : Frequency_Type;
			  Pll_Ratio : out Integer; 
			  Cd2x : out Integer)
   is
      function Ratio_For_19_2_Mhz (CDCLk : Frequency_Type) return Integer is
      begin
	 case CDClk is
	    when 172_800_000 => return 18;
	    when 192_000_000 => return 20;
	    when 307_200_000 => return 32;
	    when 326_400_000 | 652_800_000 => return 68;
	    when 556_800_000 => return 58;
	    when others => return 0;
	 end case;
      end Ratio_For_19_2_Mhz;
      function Ratio_For_24_Mhz (CDCLk : Frequency_Type) return Integer is
      begin
	 case CDClk is
	    when 172_800_000 => return 15;
	    when 192_000_000 => return 16;
	    when 307_200_000 => return 26;
	    when 326_400_000 | 652_800_000 => return 54;
	    when 556_800_000 => return 46;
	    when others => return 0;
	 end case;
      end Ratio_For_24_Mhz;
     begin
	if CDClk = 326_400_000 or CDClk = 324_000_000 then
	   CD2X := 2;
	else
	   CD2X := 1;
	end if;
	
	case Refclk is
	   when REFCLK_19_2_MHz => PLL_Ratio := Ratio_For_19_2_Mhz (CDClk);
	   when REFCLK_38_4_MHz => PLL_Ratio := Ratio_For_19_2_MHz (CDClk) / 2;
	   when REFCLK_24_MHz => PLL_Ratio := Ratio_For_24_Mhz (CDClk);
	   when others => PLL_Ratio := 0;
	end case;
     end Get_Cdclk;
     CDClk : constant Config.CDClk_Range :=
       Normalize_CDClk (Frequency_Type'Min (CDClk_In, Config.Max_CDClk));
     Success : Boolean;
   is
      begin
	 
      -- Notify the pcode that CDClk is about to change
      PCode.Mailbox_Request
	(Mbox	    => TGL_PCODE_CDCLK_CONTROL,
	 Command    => TGL_CDCLK_PREPARE_FOR_CHANGE,
	 Reply_Mask => TGL_CDCLK_READY_FOR_CHANGE,
	 Wait_Ready => True,
	 Success    => Success);
      
      if not Success then
	 pragma Debug (Debug.Put_Line
			 ("ERROR: PCODE not ready for frequency change."));
	 return;
      end if;
      
      -- Set CDCLK_PLL_ENABLE
      Registers.Set_Mask (Register => Registers.CDCLK_PLL_ENABLE,
			  Mask     => CDCLK_PLL_ENABLE_PLL_ENABLE);
      -- Poll for PLL Lock
      Registers.Wait_Set_Mask 
	(Register => Registers.CDCLK_PLL_ENABLE,
	 Mask     => CDCLK_PLL_ENABLE_PLL_LOCK);
      
      -- Write CDCLK_CTL with CD2X divider and CD Frequency Decimal value
      Get_Cdclk_Info (Refclk, CDClk, CDClk_Info);
      Registers.Unset_And_Set_Mask 
	(Register => Registers.CDCLK_CTL,
	 Unset_Mask => CDCLK_CTL_CD2X or CDCLK_CTL_CD_FREQ_SELECT_MASK or
	   CDCLK_CTL_CD2X_PIPE_MASK,
	 Set_Mask => CDCLK_Info.Freq_Select or
	   Shift_Left (CDClk_Info.CD2X_Divider, 23) or
	   Shift_Left (CDClk_Info.CD2X_Pipe, 19));
      
      -- Inform the pcode the CD clock has changed
      if CDClk <= 312_000_000 then
	 Pcode_Ctrl := 0;
      elsif CDClk <= 326_400_000 then
	 Pcode_Ctrl := 1;
      elsif Cdclk <= 556_800_000 then
	 Pcode_Ctrl := 2;
      else
	 Pcode_Ctrl := 3;
      end if;
      
      PCode.Mailbox_Write
	(MBox     => TGL_PCODE_CDCLK_CONTROL,
	 Command  => Pcode_Ctrl);
      
   end Set_CDClk;
   
   procedure Initialize is
   begin
      -- Enable PCH Reset Handshake
      Registers.Set_Mask
	(Register	=> Registers.NDE_RSTWRN_OPT,
	 Mask		=> NDE_RSTWRN_OPT_RST_PCH_Handshake_En);
      
      -- Initialize all Combo Phy DDI Buffers
      Combo_Phy.Initialize;
      
      -- Enable Power Well 1 (PG1)
      PD_On (PG1);
     
      -- Enable CD Clock following Sequences for Changing CD Clock Frequency
      
      -- Enable first DBUF
      --    set DBUF_CTL_<first_DBUF> DBUF Power Request = 1b
      --    poll for DBUF Power State = 1b (10us)
      
      -- Setup MBUS
      
      -- Program BW_BUDDY registers
   end Initialize;
end HW.GFX.GMA.Power_And_Clocks;
