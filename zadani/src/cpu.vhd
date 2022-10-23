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
    type FSMState is (state_instr_req, state_instr_wait, state_instr_decode,
                      state_data_wait, state_instr_execute, state_data_store,
                      state_io_w_wait, state_halt, state_stall,

                      state_while_ff, state_while_ff_wait,

                      state_while_fb, state_while_fb_wait, state_while_fb_pc_dec,
                      state_while_fb_stall,

                      state_dowhile_ff, state_dowhile_ff_wait,

                      state_dowhile_fb, state_dowhile_fb_wait, state_dowhile_fb_pc_dec,
                      state_dowhile_fb_stall

    );

    type Instr is (i_inc, i_dec, i_write, i_read, i_while_start, i_while_end, i_dowhile_end);

    -- MX_DATA
    signal mx_data_select: std_logic_vector(1 downto 0);

    -- MX_ADDR
    signal mx_addr_select: std_logic;
    signal mx_addr_pc: std_logic_vector(12 downto 0);  -- 00
    signal mx_addr_ptr: std_logic_vector(12 downto 0); -- 01

    -- CNT_PC
    signal cnt_pc_inc:    std_logic;
    signal cnt_pc_dec:    std_logic;

    -- CNT_PTR
    signal cnt_ptr_inc:    std_logic;
    signal cnt_ptr_dec:    std_logic;

    -- CNT_LOOP
    signal cnt_loop_inc:    std_logic;
    signal cnt_loop_dec:    std_logic;
    signal cnt_loop_reset:  std_logic;
    signal cnt_loop_cnt:        std_logic_vector(7 downto 0);

    -- FSM
    signal fsm_state: FSMState;
    signal fsm_i: Instr;
begin
    process (RESET, CLK)
    begin
        if CLK'event and CLK='1'
        then
            if RESET='1'
            then
                DATA_EN <= '0';
                DATA_RDWR <= '0';
                fsm_state <= state_stall;
                IN_REQ <= '0';
                OUT_WE <= '0';

                mx_addr_select <= '0';
                mx_data_select <= (others => '0');

                cnt_pc_inc <= '0';
                cnt_pc_dec <= '0';

                cnt_ptr_inc <= '0';
                cnt_ptr_dec <= '0';

                cnt_loop_inc <= '0';
                cnt_loop_dec <= '0';
                cnt_loop_reset <= '0';

                OUT_DATA <= (others => '0');


            elsif EN='1'
            then
                -- Fetching stateuction
                case fsm_state is
                    when state_stall =>
                        fsm_state <= state_instr_req;
                        DATA_RDWR <= '0';
                    when state_instr_req =>
                        cnt_ptr_inc <= '0';
                        cnt_ptr_dec <= '0';
                        cnt_pc_inc <= '0';
                        cnt_pc_dec <= '0';
                        cnt_loop_reset <= '0';
                        fsm_state <= state_instr_wait;
                        mx_addr_select <= '0';
                        DATA_EN <= '1';
                        DATA_RDWR <= '0';
                        IN_REQ <= '0';

                    when state_instr_wait =>
                        OUT_WE <= '0';
                        OUT_DATA <= (others => '0');
                        fsm_state <= state_instr_decode;
                        cnt_pc_inc <= '1';

                    -- Decoding stateuction
                    when state_instr_decode =>
                        cnt_pc_inc <= '0';
                        fsm_state <= state_instr_req;
                        DATA_EN <= '0';
                        case DATA_RDATA is
                            when x"3E" => -- >
                                cnt_ptr_inc <= '1';
                                fsm_state <= state_instr_req;
                            when x"3C" => -- <
                                cnt_ptr_dec <= '1';
                                fsm_state <= state_instr_req;
                            when x"2B" => -- +
                                mx_addr_select <= '1';
                                DATA_EN <= '1';
                                DATA_RDWR <= '0';
                                fsm_i <= i_inc;
                                fsm_state <= state_data_wait;
                            when x"2D" => -- -
                                mx_addr_select <= '1';
                                DATA_EN <= '1';
                                DATA_RDWR <= '0';
                                fsm_i <= i_dec;
                                fsm_state <= state_data_wait;
                            when x"2E" => -- .
                                mx_addr_select <= '1';
                                DATA_EN <= '1';
                                DATA_RDWR <= '0';
                                fsm_i <= i_write;
                                fsm_state <= state_data_wait;
                            when x"2C" => -- ,
                                IN_REQ <= '1';
                                fsm_i <= i_read;
                                fsm_state <= state_instr_execute;
                            when x"5B" => -- [
                                mx_addr_select <= '1';
                                DATA_EN <= '1';
                                DATA_RDWR <= '0';
                                fsm_i <= i_while_start;
                                fsm_state <= state_data_wait;
                            when x"5D" => -- ]
                                mx_addr_select <= '1';
                                DATA_EN <= '1';
                                DATA_RDWR <= '0';
                                fsm_i <= i_while_end;
                                fsm_state <= state_data_wait;
                            when x"29" => -- )
                                mx_addr_select <= '1';
                                DATA_EN <= '1';
                                DATA_RDWR <= '0';
                                fsm_i <= i_dowhile_end;
                                fsm_state <= state_data_wait;

                            when x"00" => -- \0
                                cnt_ptr_inc <= '0';
                                cnt_ptr_dec <= '0';
                                mx_addr_select <= '0';
                                cnt_pc_inc <= '0';
                                DATA_EN <= '0';
                                cnt_pc_dec <= '1';
                                fsm_state <= state_halt;
                            when others =>
                                -- Ignore other characters
                        end case;

                    when state_data_wait =>
                        fsm_state <= state_instr_execute;

                    when state_instr_execute =>
                        case fsm_i is
                            when i_inc =>
                                mx_data_select <= "00";
                                DATA_EN <= '1';
                                DATA_RDWR <= '1';
                                fsm_state <= state_data_store;
                            when i_dec =>
                                mx_data_select <= "01";
                                DATA_EN <= '1';
                                DATA_RDWR <= '1';
                                fsm_state <= state_data_store;
                            when i_write =>
                                if OUT_BUSY='1'
                                then
                                    fsm_state <= state_io_w_wait;
                                else
                                    OUT_DATA <= DATA_RDATA;
                                    OUT_WE <= '1';
                                    fsm_state <= state_instr_req;
                                end if;
                            when i_read =>
                                if IN_VLD='1'
                                then
                                    DATA_EN <= '1';
                                    DATA_RDWR <= '1';
                                    mx_addr_select <= '1';
                                    mx_data_select <= "10";
                                    fsm_state <= state_data_store;
                                end if;

                            when i_while_start =>
                                DATA_EN <= '0';
                                if DATA_RDATA=16#0#
                                then
                                    -- Fast forwarding to the end of the loop
                                    cnt_pc_inc <= '1';
                                    mx_addr_select <= '0';
                                    DATA_EN <= '1';
                                    DATA_RDWR <= '0';
                                    fsm_state <= state_while_ff_wait;

                                else
                                    fsm_state <= state_instr_req;
                                end if;

                            when others =>
                        end case;

                        if fsm_i=i_while_end or fsm_i=i_dowhile_end
                        then
                            DATA_EN <= '0';
                            if DATA_RDATA/=16#0#
                            then
                                -- Fast backwarding to the end of the loop
                                cnt_pc_dec <= '1';
                                mx_addr_select <= '0';
                                DATA_EN <= '1';
                                DATA_RDWR <= '0';
                                fsm_state <= state_while_fb_pc_dec;
                            else
                                fsm_state <= state_instr_req;
                            end if;
                        end if;

                    when state_while_ff_wait =>
                        fsm_state <= state_while_ff;
                        cnt_loop_inc <= '0';
                        cnt_loop_dec <= '0';
                        cnt_pc_inc <= '0';

                    when state_while_ff =>
                        -- Fast forwarding to the end of the while
                        -- Need to make sure I keep track of other
                        -- nested whiles and don't quit prematurely.

                        cnt_pc_inc <= '1';
                        cnt_pc_dec <= '0';
                        cnt_loop_inc <= '0';
                        cnt_loop_dec <= '0';

                        if DATA_RDATA = x"5B" -- [
                        then
                            cnt_loop_inc <= '1';
                            cnt_pc_dec <= '0';
                            fsm_state <= state_while_ff_wait;
                        elsif DATA_RDATA = x"5D" -- ]
                        then
                            if cnt_loop_cnt="00000000"
                            then
                                cnt_pc_dec <= '1';
                                cnt_loop_reset <= '1';
                                fsm_state <= state_instr_req;
                            else
                                cnt_loop_dec <= '1';
                                fsm_state <= state_while_ff_wait;
                            end if;
                        else
                            fsm_state <= state_while_ff_wait;
                        end if;

                    when state_while_fb_pc_dec =>
                        fsm_state <= state_while_fb_stall;
                        cnt_loop_inc <= '0';
                        cnt_loop_dec <= '0';
                        -- Decrementing instruction pointer, because it got incremented
                        -- in state_instr_decode
                        cnt_pc_dec <= '1';

                    when state_while_fb_stall =>
                        fsm_state <= state_while_fb_wait;

                    when state_while_fb_wait =>
                        fsm_state <= state_while_fb;
                        cnt_loop_inc <= '0';
                        cnt_loop_dec <= '0';
                        cnt_pc_dec <= '0';

                    when state_while_fb =>
                        -- Fast backward (lol) to the end of the while
                        -- Need to make sure I keep track of other
                        -- nested whiles and don't quit prematurely.
                        cnt_pc_inc <= '0';
                        cnt_pc_dec <= '1';
                        cnt_loop_inc <= '0';
                        cnt_loop_dec <= '0';

                        if (DATA_RDATA = x"5D" and fsm_i=i_while_end) or (DATA_RDATA = x"29" and fsm_i=i_dowhile_end) -- ] or )
                        then
                            cnt_loop_inc <= '1';
                            --cnt_pc_dec <= '0';
                            fsm_state <= state_while_fb_wait;
                        elsif (DATA_RDATA = x"5B" and fsm_i=i_while_end) or (DATA_RDATA = x"28" and fsm_i=i_dowhile_end) -- [ or )
                        then
                            if cnt_loop_cnt="00000000"
                            then
                                cnt_pc_inc <= '1';
                                cnt_pc_dec <= '0';
                                cnt_loop_reset <= '1';
                                fsm_state <= state_instr_req;
                            else
                                cnt_loop_dec <= '1';
                                fsm_state <= state_while_fb_wait;
                            end if;
                        else
                            fsm_state <= state_while_fb_wait;
                        end if;


                    when state_io_w_wait =>
                        if OUT_BUSY='1'
                        then
                            fsm_state <= state_io_w_wait;
                        else
                            OUT_DATA <= DATA_RDATA;
                            OUT_WE <= '1';
                            fsm_state <= state_instr_req;
                        end if;


                    when state_data_store =>
                        fsm_state <= state_instr_req;
                        DATA_EN <= '0';
                        DATA_RDWR <= '0';

                    when state_halt =>
                        cnt_pc_dec <= '0';
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
                 DATA_ADDR <= mx_addr_ptr + 16#1000#;
             when others =>
                 DATA_ADDR <= (others => '0');
         end case;
     end process;

     -- cnt_pc
     process (CLK, RESET, cnt_pc_inc, cnt_pc_dec, mx_addr_pc)
     begin
         if RESET='1'
         then
             mx_addr_pc <= (others => '0');
         else
             if CLK'event and CLK='1' then
                 if RESET='1' then
                     mx_addr_pc <= (others => '0');
                 end if;
                 if cnt_pc_inc='1' then
                     mx_addr_pc <= mx_addr_pc + 1;
                 end if;
                 if cnt_pc_dec='1' then
                     mx_addr_pc <= mx_addr_pc - 1;
                 end if;
             end if;
         end if;
     end process;

     -- cnt_ptr
     process (CLK, RESET, cnt_ptr_inc, cnt_ptr_dec, mx_addr_ptr)
     begin
         if RESET='1'
         then
             mx_addr_ptr <= (others => '0');
         else
             if CLK'event and CLK='1' then
                 if RESET='1' then
                     mx_addr_ptr <= (others => '0');
                 end if;
                 if cnt_ptr_inc='1' then
                     mx_addr_ptr <= mx_addr_ptr + 1;
                 end if;
                 if cnt_ptr_dec='1' then
                     mx_addr_ptr <= mx_addr_ptr - 1;
                 end if;
             end if;
         end if;
     end process;

     -- cnt_loop
     process (CLK, RESET, cnt_loop_inc, cnt_loop_dec, cnt_loop_cnt)
     begin
         if RESET='1' or cnt_loop_reset='1'
         then
             cnt_loop_cnt <= (others => '0');
         else
             if CLK'event and CLK='1' then
                 if RESET='1' then
                     cnt_loop_cnt <= (others => '0');
                 end if;
                 if cnt_loop_inc='1' then
                     if cnt_loop_cnt/=x"FF"
                     then
                         cnt_loop_cnt <= cnt_loop_cnt + 1;
                     end if;
                 end if;
                 if cnt_loop_dec='1' then
                     if cnt_loop_cnt/=x"00"
                     then
                         cnt_loop_cnt <= cnt_loop_cnt - 1;
                     end if;
                 end if;
             end if;
         end if;
     end process;

end behavioral;

