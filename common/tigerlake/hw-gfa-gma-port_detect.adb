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

with HW.GFX.GMA.Config;
with HW.GFX.GMA.Registers;
with HW.GFX.GMA.Config_Helpers;

package body HW.GFX.GMA.Port_Detect
is
   -- HOTPLUG_CTL register is used for detecting hotplug on TBT or Type-C
   --  (DP alternate mode)
   --
   -- Enable HOTPLUG_CTL and configure interrupt related registers to
   --  receive HPD IRQs.
   -- Read PORT_TX_DFLEXDPPMS DPPMSTC, PORT_TX_DFLEXDPCSSS DPPMSTC,
   --  and PORT_TX_DFLEXDPSP TC Live State.
   -- Type-C DP Alt Mode connection pending is indicated by
   --  DPSP TC Live State = 01, DPPMS = 1, DPCSSS = 0
   
   -- Legacy HDMI/DP hotplug is detected through the south display hotplug
   --  control/interrupt registers.
   
   -- If a TypeC port has a connection, then TCCold must be unblocked
   --  before accessing FIA registers (i.e. DP connect flow).
   
   function Control_TCCold (Enable : in Boolean) return Boolean
   is
      use type HW.Word64;
      Timeout : constant Time.T := Time.US_From_Now (600);
      Timed_Out : Boolean := False;
      
      Reply : Word64;
      Success : Boolean := False;
      Command : Word64 := 0;
   begin
      if Enable then 
	 Command := 1;
      end if;
      
      loop
	 pragma Loop_Invariant (not Timed_Out);
	 
	 PCode.Mailbox_Write_Read
	   (MBox => 16#8000_0026#,
	    Command => Command,
	    Reply,
	    Success);
	 exit when not Success;
	 exit when (Reply and 16#1# = 16#1#);
	 
	 Timed_Out := Time.Timed_Out (Timeout);
	 exit when Timed_Out;
      end loop;
      
      return not Timed_Out;
   end Control_TCCold;
   
   function Block_TCCold return Boolean 
   is
   begin
      return Control_TCCold (False);
   end Block_TCCold;
   
   function Unblock_TCCold return Boolean
   is
   begin
      return Control_TCCold (True);
   end Unblock_TCCold;
   
   procedure Connect_DP 
   is
   begin
      -- 1) Block TCCold
      -- 2) Read DFLEXDPPMS.DPPMSTC; if 1, then IOM has switched
      --    the lane into DP mode, else abort
      -- 3) Set DFLEXDPCSSS.DPPMSTC to 1 (disable safe mode)
      -- 4) Read DFLEXDPSP to verify port has not become disconnected
      --    If Live State is not "HPD Connect for TypeC", abort
      -- 5) Read lane assignment from DFLEXDPSP.DPX4TXLATC
      -- 6) Issue AUX reads for EDID/DPCD.
      --    Set DDI_AUX_CTL IO Select field to legacy
      --    AUX power is controlled through PWR_WELL_CTL_AUX
      -- 7) Based on lane assignment reg and EDID, software knows
      --    the max # of lanes supported.
      -- 8) Perform DP mode set enable
      -- Note that if flow is aborted at any point, clear
      --  DFLEXDPSP.DPPMSTC, then unblock TCCold
   end Connect_DP;
   
   procedure Connect_Fixed
   is
   begin
      -- 1) Block TCCold
      -- 2) Read DFLEXDPPMS.DPPMSTC, if 1 the lane is in DP mode,
      --     Otherwise abort.
      -- 3) Set DFLEXDPCSSS.DPPMSTC to 1 (disable safe mode)
      -- 4) Set PWR_WELL_CTL_AUX IO Power Request
      -- 5) Poll for PWR_WELL_CTL_AUX IO Power State to become enabled
      --     timeout after 1.5ms
      -- 6) Issue reads for EDID/DPCD
      --     Set DDI_AUX_CTL IO Select field to Legacy for DP, don't care for HDMI
      -- 7) Maximum lanes supported is known from EDID for DP, max is 4 for HDMI
      -- 8) Perform DP or HDMI mode set enable
      -- Note that if flow is aborted at any point, clear
      --  DFLEXDPSP.DPPMSTC, then unblock TCCold
   end Connect_Fixed;

   procedure Enable_Aux
   is
   begin
      -- Display must already be initialized.
      -- Enable AUX Power
      -- Disable PSR1, PSR2, GTC, DC5 and DC6
      -- Program AUX data registers
      -- Configure AUX and START transaction
      -- Wait for transaction to complete
      -- Check for errors
      -- Read AUX data register
      -- Clear status flags
   end Enable_Aux;
