library IEEE;
use IEEE.std_logic_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

library work;
use work.common.all;

entity dmem is
  port (reset : in  std_logic;
        clk   : in  std_logic;
        raddr : in  std_logic_vector(5 downto 0);
        dout  : out word;
        waddr : in  std_logic_vector(5 downto 0);
        din : in  word;
        we    : in  std_logic);
end dmem;

architecture behavioral of dmem is
  type membank_t is array (0 to 63) of word;

  signal membank0 : membank_t := (others => (others => '0'));
begin  -- architecture Behavioral

  -- purpose: create registers
  -- type   : sequential
  -- inputs : clk
  -- outputs:
  registers_proc : process (clk) is
  begin  -- process registers_proc
      if rising_edge(clk) then
          if (reset = '1') then
              membank0(0 to 63) <= (others => (others => '0'));
          elsif (we = '1') then
              membank0(to_integer(unsigned(waddr))) <= din;
          end if;
      end if;
  end process registers_proc;

  -- asynchronous read
  dout <= membank0(to_integer(unsigned(raddr)));
end behavioral;
