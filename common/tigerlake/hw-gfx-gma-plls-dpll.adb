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
	
-- DPLL0 / DPLL1: PLL available for display combo PHY usage	
-- DPLL2: TBTPLL (thunderbolt PLL)  
-- DPLL3: Not available to display  
-- DPLL4: PLL available for display combo PHY usage	
-- DDIA Clock: DDIA and DSI0 (combo port A)   
-- DDIB Clock: DDIB and DSI1 (combo port B)   
-- DDIC Clock: DDIC (combo port C)  
-- TC{1-6} Clock: DDI USBC{1-6}
   

with HW.GFX.GMA.Registers;

package body HW.GFX.GMA.PLLs.DPLL is
   
   type Regs is array (Configurable_DPLLs) of Registers.Register_Index;
   	
   type Ref_Clk_MHz is (Ref_Clk_19_2_or_38_4, Ref_Clk_24);
   type dp_pll_values is record	
	Bandwidth : DP_Bandwidth;	
	Ref_Clk   : Ref_Clk_MHz;	
	DCO_Int   : Natural;		
	DCO_Frac  : Natural;		
	PDiv      : Natural;		
	KDiv      : Natural;		
	QDiv_Mode : Natural;		
	QDiv_Ratio:Natural;		
   end record;				
   type DP_PLL_Array is array (Natural) of Dp_pll_Values;	
   DP_PLL_Values : constant DP_PLL_Array := 			
	((Bandwidth => DP_Bandwidth_5_4, 
	  Ref_Clk   => Ref_Clk_24,	 
	  DCO_Int   => 16#151#,		 
	  DCO_Frac  => 16#4000#,	 
	  PDiv      => 2,
	  KDIv      => 1,		 
	  QDiv_Mode => 0,		 
	  QDiv_Ratio=> 1),
	 (Bandwidth => DP_Bandwidth_2_7, 
	  Ref_Clk   => Ref_Clk_24,	 
	  DCO_Int   => 16#151#,		 
	  DCO_Frac  => 16#4000#,	 
	  PDiv      => 2,		 
	  KDIv      => 2,		 
	  QDiv_Mode => 0,		 
	  QDiv_Ratio=> 1),		 
	 (Bandwidth => DP_Bandwidth_1_62,
	  Ref_Clk   => Ref_Clk_24,	 
	  DCO_Int   => 16#151#,		 
	  DCO_Frac  => 16#4000#,	 
	  PDiv      => 4,		 
	  KDIv      => 2,		 
	  QDiv_Mode => 0,		 
	  QDiv_Ratio=> 1),		 
	 (Bandwidth => DP_Bandwidth_3_24,
	  Ref_Clk   => Ref_Clk_24,	 
	  DCO_Int   => 16#151#,		 
	  DCO_Frac  => 16#4000#,	 
	  PDiv      => 4,		 
	  KDIv      => 1,
	  QDiv_Mode => 0,		 
	  QDiv_Ratio=> 1),		 
	 (Bandwidth => DP_Bandwidth_2_16,
	  Ref_Clk   => Ref_Clk_24,	 
	  DCO_Int   => 16#168#,		 
	  DCO_Frac  => 0,	 
	  PDiv      => 1,		 
	  KDIv      => 2,
	  QDiv_Mode => 1,		 
	  QDiv_Ratio=> 2),		 
	 (Bandwidth => DP_Bandwidth_4_32,
	  Ref_Clk   => Ref_Clk_24,	 
	  DCO_Int   => 16#168#,
	  DCO_Frac  => 0,	 
	  PDiv      => 1,		 
	  KDIv      => 2,
	  QDiv_Mode => 0,		 
	  QDiv_Ratio=> 1),
	 (Bandwidth => DP_Bandwidth_6_48,
	  Ref_Clk   => Ref_Clk_24,	 
	  DCO_Int   => 16#195#,		 
	  DCO_Frac  => 0,
	  PDiv      => 2,		 
	  KDIv      => 1,
	  QDiv_Mode => 0,		 
	  QDiv_Ratio=> 1),		 
	 (Bandwidth => DP_Bandwidth_8_1,
	  Ref_Clk   => Ref_Clk_24,	 
	  DCO_Int   => 16#151#,		 
	  DCO_Frac  => 16#4000#,	 
	  PDiv      => 4,		 
	  KDIv      => 1,
	  QDiv_Mode => 0,		 
	  QDiv_Ratio=> 1),		 
	(Bandwidth => DP_Bandwidth_5_4,
	  Ref_Clk   => Ref_Clk_19_2_or_38_4,
	  DCO_Int   => 16#1a5#,
	  DCO_Frac  => 16#7000#,
	  PDiv      => 2,	 
	  KDIv      => 1,
	  QDiv_Mode => 0,		 
	  QDiv_Ratio=> 1),		 
	(Bandwidth => DP_Bandwidth_2_7,
	  Ref_Clk   => Ref_Clk_19_2_or_38_4,
	  DCO_Int   => 16#1a5#,
	  DCO_Frac  => 16#7000#,
	  PDiv      => 2,	 
	  KDIv      => 2,
	  QDiv_Mode => 0,		 
	  QDiv_Ratio=> 1),		 
	(Bandwidth => DP_Bandwidth_1_62,
	  Ref_Clk   => Ref_Clk_19_2_or_38_4,
	  DCO_Int   => 16#1a5#,
	  DCO_Frac  => 16#7000#,
	  PDiv      => 4,	 
	  KDIv      => 2,
	  QDiv_Mode => 0,		 
	  QDiv_Ratio=> 1),		 
	(Bandwidth => DP_Bandwidth_3_24,
	  Ref_Clk   => Ref_Clk_19_2_or_38_4,
	  DCO_Int   => 16#1a5#,
	  DCO_Frac  => 16#7000#,
	  PDiv      => 4,
	  KDIv      => 1,
	  QDiv_Mode => 0,		 
	  QDiv_Ratio=> 1),		 
	(Bandwidth => DP_Bandwidth_2_16,
	  Ref_Clk   => Ref_Clk_19_2_or_38_4,
	  DCO_Int   => 16#1c2#,
	  DCO_Frac  => 0,
	  PDiv      => 1,	 
	  KDIv      => 2,
	  QDiv_Mode => 1,		 
	  QDiv_Ratio=> 2),		 
	 (Bandwidth => DP_Bandwidth_4_32,
	  Ref_Clk   => Ref_Clk_19_2_or_38_4,
	  DCO_Int   => 16#1c2#, 
	  DCO_Frac  => 0,
	  PDiv      => 1,	 
	  KDIv      => 2,
	  QDiv_Mode => 0,		 
	  QDiv_Ratio=> 1),		 
	 (Bandwidth => DP_Bandwidth_6_48,
	  Ref_Clk   => Ref_Clk_19_2_or_38_4,
	  DCO_Int   => 16#1fa#,
	  DCO_Frac  => 16#2000#,
	  PDiv      => 2,
	  KDIv      => 1,
	  QDiv_Mode => 0,		 
	  QDiv_Ratio => 0));
		
   procedure On
     (PLL          : in Configurable_DPLLs;
      Port_Cfg     : in Port_Config;
      Success : out Boolean)
   is
      DCO_Freq : DCO_Frequency;	
      PDiv : PDiv_Range;	
      QDiv : QDiv_Range;	
      KDiv : KDiv_Range;	
      	     
      function Get_QDiv_Mode(Bandwidth : DP_Bandwidth) return Boolean is	
      begin    
          if Bandwidth = DP_Bandwidth_2_16 then	
	      return True;		   
	  else	     
	      return False;	
	  end if;    
      end Get_QDiv_Mode;	
      
   begin       
      Calculate_DPLL	
       (Port_Cfg.Mode.Dotclock, KDiv, PDiv, QDiv, QDiv_Mode, QDiv_Ratio, Success);	
      if not Success then	      
         return;
      end if;
       
      if Port_Cfg.Display = DP or Port_Cfg.Display = HDMI then
         -- Enable DPLL power and wait
         Registers.Set_Mask (Registers.DPLL_ENABLE,
                             Mask => DPLL_ENABLE_POWER_ENABLE);
         Registers.Wait_Set_Mask (Registers.DPLL_ENABLE,
                                  Mask => DPLL_ENABLE_POWER_STATE);
         
         -- Enable/Disable SSC
         if Port_Cfg.Display = DP then
            Registers.Set_Mask (Registers.DPLL_SSC,
                                Mask => DPLL_SSC_SSCEN);
         elsif Port_Cfg.Display = HDMI then
            Registers.Unset_Mask (Registers.DPLL_SSC,
                                  Mask => DPLL_SSC_SSCEN);
         end if;
             
		
	-- search through above table and find the matching fields
         -- Configure DPLL_CFGCR0
         --    DCO fraction & integer	
	 Registers.Unset_and_Set_Mask	
	      (Register => Registers.DPLL_CFGCR0,	
	       Unset_Mask => DPLL_CFGCR0_DCO_FRAC_MASK and	
		       DPLL_CFGCR0_DCO_INT_MASK,       
	       Mask => Shift_Left(Match.DCO_Frac, DCO_FRAC_SHIFT) or	
	               Shift_Left(Match.DCO_Int, DCO_INT_SHIFT));
			   
         -- Configure DPLL_CFGCR1
         --    Qdiv Ratio, Mode
         --    Kdiv, Pdiv  
	 Registers.Unset_and_Set_Mask	
		(Register => Registers.DPLL_CFGCR1,	
		 Unset_Mask => CFGCR1_QDIV_RATIO_MASK or	
			CFGCR1_QDIV_MODE_MASK or	      
			 CFGCR1_KDIV_MASK or CFGCR1_PDIV_MASK,	
		 Mask => Shift_Left(Match.QDiv_Ratio, QDIV_RATIO_SHIFT) or	
		             Shift_Left(Match.QDiv_Mode, QDIV_MODE_SHIFT) or	
			     Shift_Left(Match.KDiv, KDIV_SHIFT) or	  
			     Shift_Left(Match.PDiv, PDIV_SHIFT));	  
				     
         -- Posting reads on CFGCR{0,1}	
	 Registers.Posting_Read(Registers.DPLL_CFGCR0);	
	 Registers.Posting_Read(Registers.DPLL_CFGCR1);	
	 
	 -- DVFS Sequence Before Frequency Change (handled by CDCLk changes I think)
	 
         -- Enable DPLL in DPLL_ENABLE
         --    Set PLL Enable = 1b	
	 Registers.Set_Mask   
		(Register => Regsisters.DPLL_ENABLE,	
		  Set_Mask => DPLL_ENABLE_PLL_ENABLE);	
	       
         -- Wait for PLL Lock Status	
	Registers.Wait_Set_Mask		
		(Registers.DPLL_ENABLE,
		 Mask => DPLL_ENABLE_PLL_LOCK);
	    
         -- DVFS Sequence After Frequency Change (handled by CDCLk changes I think)	
	 -- FOr each port that will use this PLL, program DDI_CLK_SEL
         -- Configure DPLL Mapping to DDI in DPCLKA_CFGCR0   
         --   DDI* Clock Select = xy  
	 Registers.Unset_and_Set_Mask 
		(Register => Registers.DPCLKA_CFGCR0,	
		 Unset_Mask => DDI_CLOCK_SELECT_MASK,	
		 Mask => 0); -- 0 is DPLL0
		      
	 -- For each port that will use this PLL, enable the clock
         -- Turn on DDI* Clock in the DPCLKA_CFGCR0 register
         --   DDI* Clock Off = 0 (AKA turn it on)   
	 Registers.Unset_Mask 
		(Register => Registers.DPCLKA_CFGCR0,
		 Unset_Mask => DDI_CLOCK_OFF);
   end On;	 
end HW.GFX.GMA.PLLs.DPLL;	
