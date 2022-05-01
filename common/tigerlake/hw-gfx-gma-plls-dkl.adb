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

with HW.GFX.GMA.Registers;

package body HW.GFX.GMA.PLLs.DKL is
   
   type Regs is array (Configurable_DKLs) of Registers.Register_Index;
   
   procedure On
     (PLL : in Configurable_DKLs;
      Port_Cfg : in Port_Config;
      Success : out Boolean)
   is
   begin
      -- Enable PLL power in MGPLL_ENABLE
      Registers.Set_Mask (Registers.MGPLL_ENABLE,
			  Mask => DPLL_ENABLE_POWER_ENABLE);
      
      -- Wait for PLL power state enabled in MGPLL_ENABLE
      Registers.Wait_Set_Mask (Registers.MGPLL_ENABLE,
			       Mask => DPLL_ENABLE_POWER_STATE);
      
      -- Program PLL registers as in table below
      --  rate-independent first, then rate-dependent
      
      
      -- Read back the last programmed PHY PLL reg, to ensure
      --  writes complete before the next step
      -- DVFS seq before Freq Change (if applicable)
      -- Enable PLL in MGPLL_ENABLE
      -- Wait for PLL lock status in MGPLL_ENABLE (900 us)
      -- DVFS seq After Freq Change (if applicable)
      -- Program DDI_CLK_SEL to map the PLL to the port
      -- Configure DPCLKA_CFGCR0 to turn on the clock for the port
   end On;
end HW.GFX.GMA.PLLs.DKL;
