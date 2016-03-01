with Ada.Text_IO, Ada.Integer_Text_IO, Ada.Strings.Unbounded;
use Ada.Text_IO, Ada.Integer_Text_IO, Ada.Strings.Unbounded;

--//  ***
--//
--//  Author: Eric Leblanc (#948541)
--//
--//  Algorithm: credit to Moritz Lenz for the original implementation
--// of the backtracking algorithm in C++. (http://moritz.faui2k3.org/en/yasss)
--//
--//  Purpose: given a file provided by the user, takes in a partially solved
--// sudoku board (one row per line) and solves it, outputting to a file. It
--// does NOT detect unsolvable puzzles if it has more than a single solution.
--//
--//  ***

procedure sudoku is

   type board is Array(1..9, 1..9) of integer;

   main_board : board;
   infp, outfp : file_type;
   in_file_path, out_file_path : Unbounded_String;
   line : String(1..9);
   temp_char : String(1..1);
   solved : Boolean;


   --//
   --// Procedure to create a new board from an input file.
   --//
   procedure build_board(in_board : in out board) is
   begin
      -- Loop through and populate the board.
      for i in 1..9 loop
         line := get_line(infp);
         for j in 1..9 loop
            temp_char(1) := line(j);
            in_board(i, j) := Integer'Value(temp_char);
         end loop;
      end loop;

      close(infp);

   end build_board;


   --//
   --//  Function to check if a certain choice of number is valid for a
   --// given tile at coordinates (x, y).
   --//
   function is_allowed(in_board : in board; x, y, digit : in Integer) return Boolean is
      squareX, squareY : Integer;
   begin
      -- Determine which "box" the target square is in.
      squareX := ((x - 1) / 3);
      squareY := ((y - 1) / 3);

      -- Determine if move is valid horizontally and vertically.
      for i in 1..9 loop
         if (in_board(x, i) = digit or in_board(i, y) = digit) then
            return False;
         end if;
      end loop;
      -- Determine if move is valid within its box.
      for i in 1..3 loop
         for j in 1..3 loop
            if (in_board((3 * squareX) + i, (3 * squareY) + j) = digit) then
               return False;
            end if;
         end loop;
      end loop;

      return True;
   end is_allowed;


   --//
   --// Function to check if the board is solved.
   --//
   function check_board(in_board : in out Board) return Boolean is
   begin
      for i in 1..9 loop
         for j in 1..9 loop
            -- If there are zeroes remaining, we have not solved the board.
            if (in_board(i, j) = 0) then
               return False;
            end if;
         end loop;
      end loop;

      return True;
   end check_board;


   --//
   --// Prints the state of the board to standard out.
   --//
   procedure display_board(in_board : in Board) is
   begin
      put_line("+-------+-------+-------+");
      for j in 1..9 loop
         put("|");
         for i in 1..3 loop
            put(Integer'image(in_board(j, 3 * (i - 1) + 1)) &
                  Integer'image(in_board(j, 3 * (i - 1) + 2)) &
                  Integer'image(in_board(j, 3 * (i - 1) + 3)) & " |");
         end loop;
         new_line;
         if (j = 3 or j = 6 or j = 9) then
            put_line("+-------+-------+-------+");
         end if;
      end loop;
      new_line;
   end display_board;


   --//
   --// Recursive procedure to solve the board via backtracking.
   --//
   procedure solver(in_board : in out board; solved : out Boolean) is
      temp : board;
      recur_solved : Boolean;
   begin
      -- Start from the top-left, moving through columns first.
      for x in 1..9 loop
         for y in 1..9 loop
            -- Is this square populated?
            if (in_board(x, y) = 0) then
               for c in 1..9 loop
                  -- Check to see if that number is valid. If it is, continue.
                  if (is_allowed(in_board, x, y, c)) then
                     recur_solved := False;
                     temp := in_board;
                     temp(x, y) := c;
                     solver(temp, recur_solved);

                     -- Coming back out, check if the board was solved.
                     if (recur_solved or check_board(temp)) then
                        solved := True;
                        in_board := temp;
                        return;
                     end if;

                  end if;
               end loop;

               solved := False;
               return;
            end if;
         end loop;
      end loop;

      solved := False;
      return;
   end solver;


begin
   -- Get file name and open the target file.
   put("Please enter the path of the file from which we will read the board: ");
   in_file_path := To_Unbounded_String(get_line(Standard_Input));
   open(infp, in_file, To_String(in_file_path));

   -- Populate the board.
   build_board(main_board);
   solved := False;
   solver(main_board, solved);

   -- Check to see the state of the board.
   if (not solved) then
      put("This puzzle is unsolvable!");
   else
      new_line;
      put("Solved!");
      new_line;
      display_board(main_board);

      put("Please enter the path of the file to which we will output the solved board: ");
      out_file_path := To_Unbounded_String(get_line(Standard_Input));
      create(outfp, out_file, To_String(out_file_path));

      set_output(outfp);
      display_board(main_board);

      close(outfp);
   end if;
end sudoku;
