-- cpu.vhd: Simple 8-bit CPU (BrainFuck interpreter)
-- Copyright (C) 2022 Brno University of Technology,
--                    Faculty of Information Technology
-- Author(s): Ondřej Zobal <xzobal01 AT stud.fit.vutbr.cz>
--
library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;

-- ----------------------------------------------------------------------------
--                        Entity declaration
-- ----------------------------------------------------------------------------
entity cpu is
 port (
   CLK   : in std_logic;  -- hodinovy signal
   RESET : in std_logic;  -- asynchronni reset procesoru
   EN    : in std_logic;  -- povoleni cinnosti procesoru

   -- synchronni pamet RAM
   DATA_ADDR  : out std_logic_vector(12 downto 0); -- adresa do pameti
   DATA_WDATA : out std_logic_vector(7 downto 0); -- mem[DATA_ADDR] <- DATA_WDATA pokud DATA_EN='1'
   DATA_RDATA : in std_logic_vector(7 downto 0);  -- DATA_RDATA <- ram[DATA_ADDR] pokud DATA_EN='1'
   DATA_RDWR  : out std_logic;                    -- cteni (0) / zapis (1)
   DATA_EN    : out std_logic;                    -- povoleni cinnosti

   -- vstupni port
   IN_DATA   : in std_logic_vector(7 downto 0);   -- IN_DATA <- stav klavesnice pokud IN_VLD='1' a IN_REQ='1'
   IN_VLD    : in std_logic;                      -- data platna
   IN_REQ    : out std_logic;                     -- pozadavek na vstup data

   -- vystupni port
   OUT_DATA : out  std_logic_vector(7 downto 0);  -- zapisovana data
   OUT_BUSY : in std_logic;                       -- LCD je zaneprazdnen (1), nelze zapisovat
   OUT_WE   : out std_logic                       -- LCD <- OUT_DATA pokud OUT_WE='1' a OUT_BUSY='0'
 );
end cpu;


-- ----------------------------------------------------------------------------
--                      Architecture declaration
-- ----------------------------------------------------------------------------
architecture behavioral of cpu is
    type FSMState is (instr_req, instr_decode, instr_execute, instr_store, in_while, in_dowhile);
    type Instr is (i_inc, i_dec);

    -- MX_DATA
    signal mx_data_select: std_logic_vector(1 downto 0);

    -- MX_ADDR
    signal mx_addr_select: std_logic;
    signal mx_addr_pc: std_logic_vector(12 downto 0);  -- 00
    signal mx_addr_ptr: std_logic_vector(12 downto 0); -- 01

    -- CNT_PC
    signal cnt_pc_enable: std_logic;
    signal cnt_pc_inc:    std_logic;
    signal cnt_pc_dec:    std_logic;

    -- CNT_PTR
    signal cnt_ptr_enable: std_logic;
    signal cnt_ptr_inc:    std_logic;
    signal cnt_ptr_dec:    std_logic;

    -- FSM
    signal fsm_state: FSMState;
    signal fsm_instr: Instr;
begin
    process (RESET, CLK)
    begin
        if CLK'event and CLK='1'
        then
            if RESET='1'
            then
                DATA_EN <= '0';
                DATA_RDWR <= '0';
                fsm_state <= instr_req;
                IN_REQ <= '0';
                OUT_WE <= '0';

                mx_addr_select <= '0';
                mx_data_select <= (others => '0');

                cnt_pc_enable <= '0';
                cnt_pc_inc <= '0';
                cnt_pc_dec <= '0';

                cnt_ptr_enable <= '0';
                cnt_ptr_inc <= '0';
                cnt_ptr_dec <= '0';

                OUT_DATA <= (others => '0');


            else
                -- Fetching instruction
                case fsm_state is
                    when instr_req =>
                        cnt_ptr_inc <= '0';
                        cnt_ptr_dec <= '0';
                        fsm_state <= instr_decode;
                        mx_addr_select <= '0';
                        DATA_EN <= '1';

                    -- Decoding instruction
                    when instr_decode =>
                        fsm_state <= instr_req;
                        DATA_EN <= '0';
                        case DATA_RDATA is
                            when x"3E" => -- >
                                cnt_ptr_inc <= '1';
                                fsm_state <= instr_req;
                            when x"3C" => -- <
                                cnt_ptr_dec <= '1';
                                fsm_state <= instr_req;
                            when x"2B" => -- +
                            when x"2D" => -- -
                            when others =>
                        end case;
                    when others =>
                end case;
            end if;
        end if;
    end process;




    -- MX_DATA_W
    process (mx_data_select, DATA_RDATA, IN_DATA)
    begin
        case mx_data_select is
            when "00" =>
                DATA_WDATA <= DATA_RDATA + 1;
            when "01" =>
                DATA_WDATA <= DATA_RDATA - 1;
            when "10" =>
                DATA_WDATA <= IN_DATA;
            when others =>
                DATA_WDATA <= (others => '0');
        end case;
    end process;

    -- MX_ADDR
    process (mx_addr_select, mx_addr_pc, mx_addr_ptr)
    begin
        case mx_addr_select is
            when '0' =>
                DATA_ADDR <= mx_addr_pc;
            when '1' =>
                DATA_ADDR <= mx_addr_ptr;
            when others =>
                DATA_ADDR <= (others => '0');
        end case;
    end process;

    -- cnt_pc
    process (CLK, RESET, cnt_pc_enable, cnt_pc_inc, cnt_pc_dec, mx_addr_pc)
    begin
        if RESET='1'
        then
            mx_addr_pc <= (others => '0');
        else
            if CLK'event and CLK='1' then
                if RESET='1' then
                    mx_addr_pc <= (others => '0');
                end if;
                if cnt_pc_enable='1' then
                    if cnt_pc_inc='1' then
                        mx_addr_pc <= mx_addr_pc + 1;
                    end if;
                    if cnt_pc_dec='1' then
                        mx_addr_pc <= mx_addr_pc - 1;
                    end if;
                end if;
            end if;
        end if;
    end process;

    -- cnt_ptr
    process (CLK, RESET, cnt_ptr_enable, cnt_ptr_inc, cnt_ptr_dec, mx_addr_ptr)
    begin
        if RESET='1'
        then
            mx_addr_ptr <= (others => '0');
        else
            if CLK'event and CLK='1' then
                if RESET='1' then
                    mx_addr_ptr <= (others => '0');
                end if;
                if cnt_ptr_enable='1' then
                    if cnt_ptr_inc='1' then
                        mx_addr_ptr <= mx_addr_ptr + 1;
                    end if;
                    if cnt_ptr_dec='1' then
                        mx_addr_ptr <= mx_addr_ptr - 1;
                    end if;
                end if;
            end if;
        end if;
    end process;

end behavioral;

