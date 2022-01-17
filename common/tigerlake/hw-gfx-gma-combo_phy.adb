-- Copyright 2022 Tim Wawrzynczak

with GNAT.Source_Info;

with HW.Time;
with HW.Debug;
with HW.GFX.GMA.Config;
with HW.GFX.GMA.Registers;

use type HW.Word64;

package body HW.GFX.GMA.Combo_Phy is
   
   type PHY is (PHY_A, PHY_B, PHY_C, PHY_D, PHY_E, PHY_F, PHY_G, PHY_H, PHY_I);
   subtype Combo_Phy is PHY range PHY_A .. PHY_B;
   
   PHY_MISC_DE_TO_IO_COMP_PWR_DOWN	: constant := 1 * 2 ** 23;
   PORT_TX_DW8_ODCC_CLKSEL		: constant := 1 * 2 ** 31;
   PORT_TX_DW8_ODCC_DIV_SEL_MASK	: constant := 3 * 2 ** 29;
   
   type Combo_Phy_Reg is (PORT_TX_DW8_LN0,
			  PORT_TX_DW8_GRP0,
			  PORT_PCS_DW1_LN0,
			  PORT_PCS_DW1_GRP0,
			  PORT_COMP_DW3,
			  PORT_COMP_DW1,
			  PHY_MISC,
			  PORT_COMP_DW0,
			  PORT_CL_DW5);
   
   type Combo_Phy_Regs is array (Combo_Phy_Reg) of Registers_Index;
   Combo_Phy_Regs_A : constant Combo_Phy_Regs :=
     (PHY_MISC           => PHY_MISC_A,
      PORT_CL_DW5        => PORT_CL_DW5_A,
      PORT_COMP_DW0      => PORT_COMP_DW0_A,
      PORT_COMP_DW1      => PORT_COMP_DW1_A,
      PORT_COMP_DW3      => PORT_COMP_DW3_A
      PORT_TX_DW8_LN0 	 => PORT_TX_DW8_LN0_A,
      PORT_TX_DW8_GRP0   => PORT_TX_DW8_GRP0_A,
      PORT_PCS_DW1_LN0   => PORT_PCS_DW1_LN0_A,
      PORT_PCS_DW1_GRP0  => PORT_PCS_DW1_GRP0_A);
   Combo_Phy_Regs_B : constant Combo_Phy_Regs :=
     (PHY_MISC           => PHY_MISC_B,
      PORT_CL_DW5        => PORT_CL_DW5_B,
      PORT_COMP_DW0      => PORT_COMP_DW0_B,
      PORT_COMP_DW1      => PORT_COMP_DW1_B,
      PORT_COMP_DW3      => PORT_COMP_DW3_B
      PORT_TX_DW8_LN0 	 => PORT_TX_DW8_LN0_B,
      PORT_TX_DW8_GRP0   => PORT_TX_DW8_GRP0_B,
      PORT_PCS_DW1_LN0   => PORT_PCS_DW1_LN0_B,
      PORT_PCS_DW1_GRP0  => PORT_PCS_DW1_GRP0_B);
   
   -- N.B. Several Combo PHY registers come in individual lane registers and
   -- also a group registers. The group register is used exclusively for
   -- programming all of the lane registers with the same value. Reads from
   -- group address are indeterminate, so read from a single lane, then write to
   -- the group.
   procedure Initialize is
      function Combo_Phy_Reg (Phy : Combo_Phy; 
			      Reg : Combo_Phy_Reg) return Registers_Index is
	 Regs : Combo_Phy_Regs;
      begin
	 case Phy is
	    when PHY_A => return Combo_Phy_Regs_A (Reg);
	    when PHY_B => return Combo_Phy_Regs_B (Reg);
	 end case;
      end Combo_Phy_Reg;
      
      procedure Config_DCC_SusClk (Phy : Combo_Phy) is
	 DW8 : Word32;
	 DW1 : Word32;
      begin
	 --   DCC susclk divider programming
	 Registers.Read (Combo_Phy_Reg (Phy, PORT_TX_DW8_LN0), DW8);
	 DW8 := DW8 and not PORT_TX_DW8_ODCC_DIV_SEL_MASK;
	 DW8 := DW8 or PORT_TX_DW8_ODCC_CLKSEL;
	 Registers.Write (Combo_Phy_Reg (Phy, PORT_TX_DW8_GRP0), DW8);
	 
	 --   DCC discontinuous mode
	 Registers.Read (Combo_Phy_Reg (Phy, PORT_PCS_DW1_LN0), DW1);
	 DW1 := DW1 and not PORT_PCS_DW1_DCC_MODE_SELECT_MASK;  -- 3 * 2 ** 30
	 DW1 := DW1 or PORT_PCS_DW1_DCC_MODE_SELECT_CONTINUOUS; -- 3 * 2 ** 30
	 Registers.Write (Combo_Phy_Reg (Phy, PORT_PCS_DW1_GRP0), DW1);
      end Config_DCC_Susclk;
      
      procedure Config_Procmon_Reference (Phy : Combo_Phy) is
	 type Procmon_Voltage is (ZERO_85_V, ZERO_95_V, ONE_05_V);
	 type Procmon_Process is (DOT0, DOT1);
	 type Procmon_References is record
	    Voltage : Procmon_Voltage;
	    Process : Procmon_Process;
	    DW1     : Word32;
	    DW9     : Word32;
	    DW10    : Word32;
	 end record;
	 
	 ZERO_85_DOT0 : constant Procmon_References :=
	   (Voltage => ZERO_85_V,
	    Process => DOT0,
	    DW1     => 16#0000_0000#,
	    DW9     => 16#62ab_67bb#,
	    DW10    => 16#5191_4f96#);
	 ZERO_95_DOT0 : constant Procmon_References :=
	   (Voltage => ZERO_95_V,
	    Process => DOT0,
	    DW1     => 16#0000_0000#,
	    DW9     => 16#86e1_72c7#,
	    DW10    => 16#77ca_5eab#);
	 ZERO_95_DOT1 : constant Procmon_References :=
	   (Voltage => ZERO_95_V,
	    Process => DOT1,
	    DW1     => 16#0000_0000#,
	    DW9     => 16#93f8_7fe1#,
	    DW10    => 16#8ae8_71c5#);
	 ONE_05_DOT0 : constant Procmon_References :=
	   (Voltage => ONE_05_V,
	    Process => DOT0,
	    DW1     => 16#0000_0000#,
	    DW9     => 16#98fa_82dd#,
	    DW10    => 16#89e4_6dc1#);
	 ONE_05_DOT1 : constant Procmon_References :=
	   (Voltage => ONE_05_V,
	    Process => DOT1,
	    DW1     => 16#0044_0000#,
	    DW9     => 16#9a00_ab25#,
	    DW10    => 16#8ae3_8ff1#);
	 procedure Read_DW3 (Phy : Combo_Phy; References : out Procmon_References) is
	    DW3 : Word32;
	    Process : Word32;
	    Voltage : Word32;
	    PROCESS_MASK : constant := 7 * 2 ** 26;
	    VOLTAGE_MASK : constant := 3 * 2 ** 24;
	 begin
	    Registers.Read (Register => Combo_Phy_DW3_Reg (Phy), DW3);
	    Tmp := DW3 and VOLTAGE_MASK;
	    case Tmp is
	       when 16#0000_0000# => Voltage := ZERO_85_V;
	       when 16#0100_0000# => Voltage := ZERO_95_V;
	       when 16#0200_0000# => Voltage := ONE_05_V;
	       when others        => Voltage := ZERO_85_V;
	    end case;
	    
	    Tmp := DW3 and PROCESS_MASK;
	    case Tmp is
	       when 16#0400_0000# => Process := DOT0;
	       when 16#0800_0000# => Process := DOT1;
	       when others =>        Process := DOT0;
	    end case;
	    
	    if Process = DOT0 then
	       if Voltage = ZERO_85_V then
		  References := ZERO_85_DOT0;
	       elsif Voltage = ZERO_95_V then
		  References := ZERO_95_DOT0;
	       elsif Voltage = ONE_05_V then
		  References := ONE_05_DOT0;
	       end if;
	    elsif Process = DOT1 then
	       if Voltage = ZERO_95_V then
		  References := ZERO_95_DOT1;
	       elsif Voltage = ONE_05_V then
		  References := ONE_05_DOT1;
	       else -- default
		  References := ZERO_95_DOT1;
	       end if;
	    end case;
	 end Read_DW3;
	 References : Procmon_References;
      begin
	 Read_DW3(Phy, References);
	 Registers.Unset_And_Set_Mask (Register => Combo_Phy_Reg (Phy, PORT_COMP_DW1),
				        -- not (shift_right(16#ff#, 16) or 16#ff)
				       Unset_Mask => PORT_COMP_DW1_REF_MASK,
				       Set_Mask => References.DW1);
	 Registers.Write (Register => Combo_Phy_Reg (Phy, PORT_COMP_DW9),
			  Value    => References.DW9);
	 Registers.Write (Register => Combo_Phy_Reg (Phy, PORT_COMP_DW10),
			  Value    => References.DW10);
      end Config_Procmon_Reference;
      
      function Phy_Is_Master (Phy : Combo_Phy) return Boolean is begin
	 case Phy of
	    when PHY_A => return True;
	    when others => return False;
	 end case;
      end Phy_Is_Master;
   begin
      -- Initialize all combo PHYs with Combo PHY DDI Buffer Combo PHY Init Sequence
      for Phy in Combo_Phy'range loop
	 Config_DCC_SusClk (Phy);
	 
	 --   Clear PHY_MISC_<DDI that maps to this PHY> DE to IO Comp Pwr Down to 0b.
	 Registers.Unset_Mask (Combo_Phy_Reg (Phy, PHY_MISC),
			       PHY_MISC_DE_TO_IO_COMP_PWR_DOWN);
	 
	 --   Program procmon reference values in PORT_COMP_DW{1,9,10}
	 Config_Procmon_Reference (Phy);
	 
	 --   If this PHY is a comp source, set DW8 irefgen to 1
	 if Phy_Is_Master (Phy) then
	    Registers.Set_Mask (Register => Combo_Phy_Reg (Phy, PORT_COMP_DW8),
				Set_Mask => COMP_DW8_IREFGEN); -- 1 * 2 ** 24
	 end if;
	 
	 --   Set PORT_COMP_DW0 Comp Init to 1
	 Registers.Set_Mask (Register => Combo_Phy_Reg (Phy, PORT_COMP_DW0),
			     Set_Mask => PORT_COMP_DW0_COMP_INIT);
	 
	 --   PORT_CL_DW5 CL Power Down Enable to 1
	 Registers.Set_Mask (Register => Combo_Phy_Reg (Phy, PORT_CL_DW5),
			     Set_Mask => PORT_CL_DW5_POWER_DOWN_ENABLE);
      end loop;

   end Initialize;
end HW.GFX.GMA.Combo_Phy;
