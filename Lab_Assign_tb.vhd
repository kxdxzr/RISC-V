library IEEE;
use IEEE.std_logic_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

library work;
use work.common.all;

entity Lab_Assign_tb is
end Lab_Assign_tb;

architecture behavioral of Lab_Assign_tb is

constant clk_period : time := 10 ns;
signal clk : std_logic;
signal reset : std_logic;
signal cpuout : word;

component Lab_Assign is
	port (reset : in  std_logic;
				clk   : in  std_logic;
				y : out word);
end component;

begin

	 u0: Lab_Assign port map(
				reset => reset,
				clk => clk,
				y => cpuout);

    proc_clock: process
    begin
        clk <= '0';
        wait for clk_period/2;
        clk <= '1';
        wait for clk_period/2;
    end process;

    proc_stimuli: process
    begin
        reset <= '0';
		wait for clk_period * 17; -- end of data input
			assert (cpuout = x"00000000") report "error  value(0): "
			& integer'image(to_integer(unsigned(cpuout))) severity failure;
		wait for clk_period * 44; -- each cycle of the loop takes 44 cycles
			-- testing result 0
			assert (cpuout = x"F0793F8D") report "error  value(0): "
			& integer'image(to_integer(unsigned(cpuout))) severity failure;
		wait for clk_period * 44; -- testing result 1
			assert (cpuout = x"93F8DAF1") report "error value(1): "
			& integer'image(to_integer(unsigned(cpuout))) severity failure;
		wait for clk_period * 44; -- testing result 2
			assert (cpuout = x"8DAF1114") report "error value(2): "
			& integer'image(to_integer(unsigned(cpuout))) severity failure;
		wait for clk_period * 44; -- testing result 3
			assert (cpuout = x"F1114DF1") report "error value(3): "
			& integer'image(to_integer(unsigned(cpuout))) severity failure;
		wait for clk_period * 44; -- testing result 4
			assert (cpuout = x"14DF1B15") report "error value(4): "
			& integer'image(to_integer(unsigned(cpuout))) severity failure;
		wait for clk_period * 44; -- testing result 5
			assert (cpuout = x"F1B15EF7") report "error value(5): "
			& integer'image(to_integer(unsigned(cpuout))) severity failure;
		wait for clk_period * 44; -- testing result 6
			assert (cpuout = x"15EF7B4D") report "error value(6): "
			& integer'image(to_integer(unsigned(cpuout))) severity failure;
		wait for clk_period * 44; -- testing result 7
			assert (cpuout = x"F7B4DF50") report "error value(7): "
			& integer'image(to_integer(unsigned(cpuout))) severity failure;
		wait for clk_period * 44; -- testing result 8
			assert (cpuout = x"4DF50D8B") report "error value(8): "
			& integer'image(to_integer(unsigned(cpuout))) severity failure;
		wait for clk_period * 44; -- testing result 9
			assert (cpuout = x"50D8BD88") report "error value(9): "
			& integer'image(to_integer(unsigned(cpuout))) severity failure;
		wait for clk_period * 44; -- testing result 10
			assert (cpuout = x"8BD888ED") report "error value(10): "
			& integer'image(to_integer(unsigned(cpuout))) severity failure;
		wait for clk_period * 44; -- testing result 11
			assert (cpuout = x"888ED358") report "error value(11): "
			& integer'image(to_integer(unsigned(cpuout))) severity failure;
		wait for clk_period * 44; -- testing result 12
			assert (cpuout = x"ED358A5C") report "error value(12): "
			& integer'image(to_integer(unsigned(cpuout))) severity failure;
		wait for clk_period * 44; -- testing result 13
			assert (cpuout = x"58A5CB8F") report "error value(13): "
			& integer'image(to_integer(unsigned(cpuout))) severity failure;
		wait for clk_period * 44; -- testing result 14
			assert (cpuout = x"5CB8FC3A") report "error value(14): "
			& integer'image(to_integer(unsigned(cpuout))) severity failure;
		wait for clk_period * 44; -- testing result 15
			assert (cpuout = x"8FC3A867") report "error value(15): "
			& integer'image(to_integer(unsigned(cpuout))) severity failure;
		wait for clk_period * 44; -- testing result 16
			assert (cpuout = x"3A867F59") report "error value(16): "
			& integer'image(to_integer(unsigned(cpuout))) severity failure;
		wait for clk_period * 44; -- testing result 17
			assert (cpuout = x"67F59EAD") report "error value(17): "
			& integer'image(to_integer(unsigned(cpuout))) severity failure;
		wait for clk_period * 44; -- testing result 18
			assert (cpuout = x"59EADA84") report "error value(18): "
			& integer'image(to_integer(unsigned(cpuout))) severity failure;
		wait for clk_period * 44; -- testing result 19
			assert (cpuout = x"ADA84057") report "error value(19): "
			& integer'image(to_integer(unsigned(cpuout))) severity failure;
		wait for clk_period * 7; -- finalizing
    assert false report "success - end of simulation" severity failure;
    end process;
end architecture;
