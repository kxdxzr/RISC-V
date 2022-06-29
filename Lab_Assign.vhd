-- execute I and R type instructions

library IEEE;
use IEEE.std_logic_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

library work;
use work.common.all;

entity Lab_Assign is
    port (reset : in  std_logic;
          clk   : in  std_logic;
			    y : out word);
end Lab_Assign;

architecture behavioral of Lab_Assign is

signal alu_func : alu_func_t := ALU_NONE;
signal alu_A : word := x"00000000";
signal alu_B : word := x"00000000";
signal alu_out : word := x"00000000";
signal reg_A : word := x"00000000";
signal reg_B : word := x"00000000";
signal imm : word := x"00000000";
signal imm_rd : word := x"00000000";
signal ir : word := x"00000000";
signal dmem_out : word := x"00000000";
signal rf_wdata : word := x"00000000";
signal branch_imm : unsigned(word'range) := x"00000000";
signal Jump_imm : unsigned(word'range) := x"00000000";
signal brjmp : unsigned(word'range) := x"00000000";
signal jalr_cal: unsigned(word'range) := x"00000000";
signal pc_buffer : unsigned(word'range) := x"000000A4";
--need to be changed later
signal imm_u : word := x"00000000";
-- instruction fields
signal opcode : opcode_t;
signal funct3 : std_logic_vector(2 downto 0);
signal funct7 : std_logic_vector(6 downto 0);
signal rs1 : std_logic_vector(4 downto 0);
signal rs2 : std_logic_vector(4 downto 0);
signal rd : std_logic_vector(4 downto 0);
signal pc : unsigned(word'range) := x"000000A4"; --pc is modified to make it
-- starts at the position of main

-- control signals
signal regwrite : std_logic;
signal wbsel :  std_logic_vector(1 downto 0);
signal memwrite : std_logic;
signal op2sel : std_logic_vector(2 downto 0);
signal pcsel: std_logic_vector(1 downto 0) ;
signal op1sel : std_logic_vector(1 downto 0);


--intermediate signals
signal reg_A_out : word := x"00000000";
signal reg_B_out : word := x"00000000";
signal imm_out : word := x"00000000";
signal imm_u_out : word := x"00000000";
signal imm_rd_out : word := x"00000000";
signal alu_out_out : word := x"00000000";
signal branch_imm_out : unsigned(word'range) := x"00000000";
signal Jump_imm_out : unsigned(word'range) := x"00000000";
signal op1sel_out : std_logic_vector(1 downto 0);
signal op2sel_out : std_logic_vector(2 downto 0);
signal memwrite_out : std_logic;
signal regwrite_out : std_logic;
signal wbsel_out : std_logic_vector(1 downto 0);
signal pcsel_out : std_logic_vector(1 downto 0);
signal rd_out : std_logic_vector(4 downto 0);
signal alu_func_out : alu_func_t := ALU_NONE;
signal op1equ : std_logic;
signal op2equ : std_logic;

-- Branch signals
signal br_neq : std_logic_vector(1 downto 0) ;

--output
signal regbank_out : regbank_t;
component alu is
port (alu_func : in  alu_func_t;
		op1      : in  word;
		op2      : in  word;
		result   : out word);
end component alu;

component imem is
port(
	addr : in std_logic_vector(6 downto 0);
	dout : out word);
end component imem;

component dmem is
port (reset : in  std_logic;
      clk   : in  std_logic;
      raddr : in  std_logic_vector(5 downto 0);
      dout  : out word;
      waddr : in  std_logic_vector(5 downto 0);
      din : in  word;
      we    : in  std_logic);
end component dmem;

component regfile is
port (reset : in  std_logic;
      clk   : in  std_logic;
      addra : in  std_logic_vector(4 downto 0);
      addrb : in  std_logic_vector(4 downto 0);
      rega  : out word;
      regb  : out word;
      addrw : in  std_logic_vector(4 downto 0);
      dataw : in  word;
      we    : in  std_logic;
      regbank0_out : out regbank_t);
end component regfile;

component interme_reg is
port (reset : in  std_logic;
      clk   : in  std_logic;
      datain : in  word;
      dataout : out word);
end component interme_reg;

component interme5_control is
port (reset : in  std_logic;
      clk   : in  std_logic;
      addrin : in std_logic_vector(4 downto 0);
      addrout : out std_logic_vector(4 downto 0));
end component interme5_control;

component interme4_control is
port (reset : in  std_logic;
      clk   : in  std_logic;
      addrin : in std_logic_vector(3 downto 0);
      addrout : out std_logic_vector(3 downto 0));
end component interme4_control;

component interme3_control is
    port (reset : in  std_logic;
          clk   : in  std_logic;
          addrin : in std_logic_vector(2 downto 0);
          addrout : out std_logic_vector(2 downto 0));
end component interme3_control;

component interme2_control is
port (reset : in  std_logic;
      clk   : in  std_logic;
      addrin : in std_logic_vector(1 downto 0);
      addrout : out std_logic_vector(1 downto 0));
end component interme2_control;

component interme1_control is
port (reset : in  std_logic;
      clk   : in  std_logic;
      addrin : in std_logic;
      addrout : out std_logic);
end component interme1_control;

component interme_reg_imm is
    port (reset : in  std_logic;
          clk   : in  std_logic;
          datain : in  unsigned(word'range);
          dataout : out unsigned(word'range));
end component interme_reg_imm;

begin
	-- datapath
	alu0: alu port map(
		alu_func => alu_func_out,
            op1 => alu_A,
            op2 => alu_B,
			result => alu_out);

	imem0: imem port map(
        	addr => std_logic_vector(pc(8 downto 2)),
        	dout => ir);

	dmem0: dmem port map(
	  reset => reset,
		clk => clk,
    raddr => alu_out(7 downto 2),
		dout => dmem_out,
		waddr => alu_out(7 downto 2),
		din => reg_B_out,
		we => memwrite_out);

	rf0: regfile port map(
	   reset => reset,
		clk => clk,
        addra => rs1,
        addrb => rs2,
    rega => reg_A,
		regb => reg_B,
		addrw => rd_out,
		dataw => rf_wdata,
		we => regwrite_out,
    regbank0_out => regbank_out);

-- intermediate registers \|/

    rega_int : interme_reg port map(
    reset => reset,
    clk   => clk,
    datain => reg_A,
    dataout => reg_A_out);

    regb_int : interme_reg port map(
    reset => reset,
    clk   => clk,
    datain => reg_B,
    dataout => reg_B_out);

    reg_imm : interme_reg port map(
    reset => reset,
    clk   => clk,
    datain => imm,
    dataout => imm_out);

    reg_imm_u : interme_reg port map(
    reset => reset,
    clk   => clk,
    datain => imm_u,
    dataout => imm_u_out);

    reg_imm_rd : interme_reg port map(
    reset => reset,
    clk   => clk,
    datain => imm_rd,
    dataout => imm_rd_out);

    reg_alu_out : interme_reg port map(
    reset => reset,
    clk   => clk,
    datain => alu_out,
    dataout => alu_out_out);

    reg_branch_imm : interme_reg_imm port map(
    reset => reset,
    clk   => clk,
    datain => branch_imm,
    dataout => branch_imm_out);

    reg_Jump_imm : interme_reg_imm port map(
    reset => reset,
    clk   => clk,
    datain => Jump_imm,
    dataout => Jump_imm_out);

    op1sel_reg : interme2_control port map(
    reset => reset,
    clk   => clk,
    addrin => op1sel,
    addrout => op1sel_out);

    op1se2_reg : interme3_control port map(
    reset => reset,
    clk   => clk,
    addrin => op2sel,
    addrout => op2sel_out);

    memwrite_reg : interme1_control port map(
    reset => reset,
    clk   => clk,
    addrin => memwrite,
    addrout => memwrite_out);

    regwrite_reg : interme1_control port map(
    reset => reset,
    clk   => clk,
    addrin => regwrite,
    addrout => regwrite_out);

    wbsel_reg : interme2_control port map(
    reset => reset,
    clk   => clk,
    addrin => wbsel,
    addrout => wbsel_out);

    pcsel_reg : interme2_control port map(
    reset => reset,
    clk   => clk,
    addrin => pcsel,
    addrout => pcsel_out);

    alu_func_t_reg : interme4_control port map(
    reset => reset,
    clk   => clk,
    addrin => alu_func,
    addrout => alu_func_out);

    rd_reg : interme5_control port map(
    reset => reset,
    clk   => clk,
    addrin => rd,
    addrout => rd_out);


-- intermediate registers /|\

    alu_B <= reg_B_out when op2sel_out = "000" else
    			imm_out when op2sel_out = "001" else
				  imm_rd_out when op2sel_out = "010" else
    			std_logic_vector(jalr_cal) when op2sel_out = "011" else
          imm_u_out when op2sel_out = "100" else
          x"00000000" when op2sel_out = "101" else
          alu_out_out when op2sel_out = "110" else x"00000000";
    alu_A <= reg_A_out when op1sel_out = "00" else
          x"00000000" when op1sel_out = "01" else
          std_logic_vector(PC) when op1sel_out = "10" else
          alu_out_out when op1sel_out = "11";

    rf_wdata <= alu_out when wbsel_out = "00" else
          dmem_out when wbsel_out = "01" else std_logic_vector(pc_buffer);
   -- Branch condition gen
	br_neq <= "00" when (reg_A = reg_B) else
			 "01";
  op1equ <= '1' when ((rs1 = rd_out) and rd_out /= "00000") else
      '0';
  op2equ <= '1' when ((rs2 = rd_out) and rd_out /= "00000") else
      '0';

  jalr_cal <= unsigned(reg_A_out) + unsigned(imm_out);
	-- instruction fields
	imm(31 downto 12) <= (others => ir(31));
	imm(11 downto 0) <= ir(31 downto 20);
	imm_rd(31 downto 12) <= (others => funct7(6));
	imm_rd(11 downto 5) <= funct7;
	imm_rd(4 downto 0) <= rd;
   rs1 <= ir(19 downto 15);
   rs2 <= ir(24 downto 20);
	rd <= ir(11 downto 7);
	funct3 <= ir(14 downto 12);
	funct7 <= ir(31 downto 25);
	opcode <= ir(6 downto 0);
  imm_u(31 downto 12) <= ir(31 downto 12); --U-type
  imm_u(11 downto 0) <= (others => '0');
	branch_imm(31 downto 13) <= (others => ir(31)); --SB-type
	branch_imm(12 downto 0) <= unsigned(ir(31) & ir(7) &
								ir(30 downto 25) & ir(11 downto 8) & '0');
  Jump_imm(31 downto 20) <= (others => ir(31)); --
  Jump_imm(19 downto 0) <= unsigned(ir(19 downto 12) & ir(20) &
                ir(30 downto 21) & '0'); -- this is for J type
  y <= regbank_out(12); --output the result for verification

   decode_proc : process (ir, funct7, funct3, opcode, br_neq, op1equ, op2equ, wbsel_out) is
	begin
    op1sel <= "00";
    regwrite <= '0';
		op2sel <= "000";
		memwrite <= '0';
		wbsel <= "00";
		pcsel <= "00";
		alu_func <= ALU_NONE;
    pc_buffer <= pc + 4;

		case opcode is
			when OP_ITYPE =>
				regwrite <= '1';
				op2sel <= "001";
				pcsel <= "00";
        if (op1equ = '1' and regwrite_out = '1') then
           op1sel <= "11";
        else
           op1sel <= "00";
        end if;
				case (funct3) is
                    when "000" => alu_func <= ALU_ADD;
                    when "001" => alu_func <= ALU_SLL;
                    when "010" => alu_func <= ALU_SLT;
                    when "011" => alu_func <= ALU_SLTU;
                    when "100" => alu_func <= ALU_XOR;
                    when "110" => alu_func <= ALU_OR;
                    when "111" => alu_func <= ALU_AND;
                    when "101" =>
                        if (ir(30) = '1') then
                            alu_func <= ALU_SRA;
                        else
                            alu_func <= ALU_SRL;
                        end if;

                    when others => null;
                end case;

			when OP_RTYPE =>
				regwrite <= '1';
				pcsel <= "00";
        if (op2equ = '1' and regwrite_out = '1') then
           op2sel <= "110";
        else
           op2sel <= "000";
        end if;
        if (op1equ = '1' and regwrite_out = '1') then
           op1sel <= "11";
        else
           op1sel <= "00";
        end if;
				case (funct3) is
					when "000" =>
						if (ir(30) = '1') then
							 alu_func <= ALU_SUB;
						else
							 alu_func <= ALU_ADD;
						end if;
					when "001" => alu_func <= ALU_SLL;
					when "010" => alu_func <= ALU_SLT;
					when "011" => alu_func <= ALU_SLTU;
					when "100" => alu_func <= ALU_XOR;
					when "101" =>
						if (ir(30) = '1') then
							 alu_func <= ALU_SRA;
						else
							 alu_func <= ALU_SRL;
						end if;
					when "110"  => alu_func <= ALU_OR;
					when "111"  => alu_func <= ALU_AND;
					when others => null;
				end case;

			when OP_STORE =>
				memwrite <= '1';
				op2sel <= "010"; -- Select offset as alu operand
				pcsel <= "00";
        if (op1equ = '1' and regwrite_out = '1') then
           op1sel <= "11";
        else
           op1sel <= "00";
        end if;
				case (funct3) is
					when "000" => alu_func <= ALU_ADD; -- SB
					when "001" => alu_func <= ALU_ADD; -- SH
					when "010" => alu_func <= ALU_ADD; -- SW
					when others => null;
				end case;

			when OP_BRANCH =>
				regwrite <= '0';
				alu_func <= ALU_NONE;
				case (funct3) is
					when "001" => -- BNE
						pcsel <= br_neq;
					when others => null;
				end case;

      when OP_JAL =>
          wbsel <= "10";
          regwrite <= '1';
          pcsel <= "11";
          op2sel <= "001";

      when OP_JALR =>
          wbsel <= "10";
          regwrite <= '1';
          pcsel <= "10";
          op2sel <= "001"; -- Select offset as alu operand

      when OP_LUI =>
          op1sel <= "01";
          op2sel <= "100";
          regwrite <= '1';
          alu_func <= ALU_ADD;

      when OP_LOAD =>
          regwrite <= '1';
  				op2sel <= "001"; -- Select offset as alu operand
  				pcsel <= "00";
          wbsel <= "01";
  				case (funct3) is
  					when "000" => alu_func <= ALU_ADD; -- LB
  					when "001" => alu_func <= ALU_ADD; -- LH
  					when "010" => alu_func <= ALU_ADD; -- LW
  					when others => null;
  				end case;

      when OP_AUIPC =>
          op1sel <= "10";
          op2sel <= "100";
          regwrite <= '1';
          alu_func <= ALU_ADD;


			when others => null;
		end case;
    end process;

	acc: process(reset, clk)
	begin
    if (reset = '1') then
      pc <= (others => '0');
    elsif rising_edge(clk) then
      if(pcsel_out = "00") then
        pc <= pc_buffer; --normal +4
      elsif (pcsel_out = "01") then
        pc <= pc + branch_imm_out; -- Take the branch
      elsif (pcsel_out = "10") then
        pc <= unsigned(jalr_cal); --jalr
      else
        pc <= pc + Jump_imm_out; --jal
      end if;
		end if;
	end process;

end architecture;
