! A PROGRAM TO PLAY TIC-TAC-TOE
! MODIFIED AND MODERNIZED 01/25/2016

      program tictactoe
      implicit none

      character (len=1), dimension (3,3) :: tictac
      character (len=1) :: winner
      logical :: over, check_play
      integer :: move

      write(*,*) "PLAY TIC-TAC-TOE. ENTER 1-9 TO PLAY"
      write(*,*) " "
      write(*,*) "        1 | 2 | 3 "
      write(*,*) "       ---+---+---"
      write(*,*) "        4 | 5 | 6 "
      write(*,*) "       ---+---+---"
      write(*,*) "        7 | 8 | 9 "
      write(*,*) " "
            
      tictac = " "
      


! START THE LOOP CONTROLLING EACH TURN

      do 
        write(*,*) "Your move? "
        read(*,*) move
        if (move < 1 .OR. move > 9) then
          write (*,*) "Invalid input."
          cycle
        end if
        if (.NOT. check_play(tictac, move)) then
          write (*,*) "Invalid move, box already occupied."
          cycle
        end if

        select case (move) 
          case (1)
              tictac(1,1) = "X"
          case (2)
              tictac(1,2) = "X"
          case (3)
              tictac(1,3) = "X"
          case (4)
              tictac(2,1) = "X"
          case (5)
              tictac(2,2) = "X"
          case (6)
              tictac(2,3) = "X"
          case (7)
              tictac(3,1) = "X"
          case (8)
              tictac(3,2) = "X"
          case (9)
              tictac(3,3) = "X"
        end select
     
        ! Apply the player's move.
        write (*,*) "After your move..."
        call draw_board(tictac)
        call check_over(tictac, over, winner)
        
        ! If the game isn't over, play the AI's turn.
        if (.NOT. over) then

          write (*,*) "After my move..."
          call computer_move(tictac)
          call draw_board(tictac)
          call check_over(tictac, over, winner)
        
        end if
      
        ! Game over?
        if (over) then
      
          write (*,*) "The game is over!"
          if (winner .EQ. "D") then
            write(*,*) "The game is a draw. "
          else
            write (*,*) "The winner is: ", winner
          end if     
          exit

        end if

      end do
      
      end
      
! SUBROUTINE TO CHECK TO SEE IF THE GAME IS OVER      
!=========================================
      subroutine check_over(tictac, over, winner)
      implicit none

      logical, intent (out) :: over
      logical :: same, dsame
      character (len=1), dimension (3,3), intent (inout) :: tictac
      character (len=1), parameter :: BLANK = ' ', DRAW = 'D'
      character (len=1), intent (out) :: winner
      integer :: irow, icol

! ASSUME GAME IS OVER AT START
      over = .TRUE.

! CHECK FOR A WINNER
! CHECK ROWS FOR A WINNER
      do irow = 1, 3
        if (same(tictac(irow, 1), tictac(irow, 2), tictac(irow, 3))) then
          winner = tictac(irow, 1)
          return
        end if
      end do

! NO WINNER BY ROWS, CHECK COLUMNS FOR A WINNER
      do icol = 1, 3
        if (same(tictac(1, icol), tictac(2, icol), tictac(3, icol))) then
          winner = tictac(1, icol)
          return
        end if
      end do 

! NO WINNER BY ROWS OR COLUMNS, CHECK DIAGONALS
      dsame = same(tictac(1, 1), tictac(2, 2), tictac(3, 3)) &
                .OR. same(tictac(1, 3), tictac(2, 2), tictac(3, 1)) 
      if (dsame) then
        winner = tictac(2, 2)
        return
      end if

! NO WINNER AT ALL. SEE IF GAME IS A DRAW
! CHECK EACH ROW FOR AN EMPTY SPACE
      do irow = 1, 3
        do icol = 1, 3
          if (tictac(irow, icol) .EQ. BLANK) then
            over = .FALSE.
            return
          end if
        end do
      end do

! NO BLANK FOUND, GAME IS A DRAW
      winner = DRAW

      return
      end
      

! SUBROUTINE TO PLAY FOR THE COMPUTER  
! =========================================
      subroutine computer_move(tictac)
      implicit none

      character (len=1), dimension (3,3), intent (inout) :: tictac
      real :: randseed
      integer, dimension (3,8) :: paths = reshape((/1,2,3,4,5,6,7,8,9, &
                                                    1,4,7,2,5,8,3,6,9, &
                                                    1,5,9,3,5,7 &
                                                   /), &
                                                    (/3,8/))
      integer, dimension (9,2) :: board = reshape((/1,1,1,2,2,2,3,3,3, &
                                                    1,2,3,1,2,3,1,2,3 &
                                                   /), &
                                                  (/9,2/))
      integer, dimension (8) :: pathsum
      integer :: i, j, k, x, y, randpos

      
!     CALCULATE THE PATHSUMS
      do i = 1, 8
        pathsum(i) = 0
        do j = 1, 3
          x = board(paths(j, i),1)
          y = board(paths(j, i),2)
          if (tictac(x, y) .EQ. " ") k = 0
          if (tictac(x, y) .EQ. "X") k = 1
          if (tictac(x, y) .EQ. "O") k = 4 
          pathsum(i) = pathsum(i) + k     
        end do
      end do

!     OFFENSIVE CODE TO DEAL WITH SCENARIOS WHERE THE
!     COMPUTER HAS TWO IN A PATH
      do i = 1, 8
        if (pathsum(i) .EQ. 8) then
          do j = 1, 3
            x = board(paths(j, i), 1)
            y = board(paths(j, i), 2)
            if (tictac(x, y) .EQ. " ") then
              tictac(x, y) = "O"
              return
            end if
          end do
        end if
      end do
  
!     DEFENSIVE CODE TO DEAL WITH SCENARIOS WHERE THE
!     OPPONENT HAS TWO IN A PATH
      do i = 1, 8
        if (pathsum(i) .EQ. 2) then
          do j = 1, 3
            x = board(paths(j,i), 1)
            y = board(paths(j,i), 2)
            if (tictac(x, y) .EQ. " ") then
              tictac(x, y) = "O"
              return
            end if
          end do
        end if
      end do
  
!     OTHERWISE, RANDOM MOVE
      call random_seed()
      do
        call random_number(randseed)
        randpos = int(randseed * 9) + 1
        x = board(randpos, 1)
        y = board(randpos, 2)
        if (tictac(x, y) .EQ. " ") then
            tictac(x, y) = "O"
            return
        end if
      end do
  
      return    
      end

! FUNCTION TO CHECK TO SEE IF THREE ELEMENTS IN A ROW, COLUMN OR DIAGONAL
! ARE THE SAME           
! =========================================
      logical function same(t1, t2, t3)
      implicit none

      character, intent (in) :: t1, t2, t3
      
      ! Assume false.
      same = .FALSE.

      if (t1 .EQ. "X" .AND. t2 .EQ. "X" .AND. t3 .EQ. "X") &
                same = .TRUE. 
      if (t1 .EQ. "O" .AND. t2 .EQ. "O" .AND. t3 .EQ. "O") &
                same = .TRUE.
      return
      end

! SUBROUTINE TO DRAW THE BOARD
! =========================================
      subroutine draw_board(tictac)
      implicit none

      character (len=1), dimension (3,3), intent (inout) :: tictac
      integer :: i, j

      do i=1, 3
        write (*,400) (tictac(i, j), j=1,3)
        400 format(2X, A1, 1X, "|", 1X, A1, 1X, "|", 1X, A1, 1X)
        ! Only draw the following twice.
        if (i .NE. 3) write (*,*) "---+---+---"
      end do

      return 
      end

! SUBROUTINE TO CHECK HUMAN PLAY  
! ========================================= 
      logical function check_play(tictac, move) 
      implicit none

      character (len=1), dimension (3, 3), intent (in) :: tictac
      integer, intent (in) :: move

      ! Assume false.
      check_play = .FALSE.

                
      select case (move)
      case (1)
        if (tictac(1, 1) .EQ. " ") check_play = .TRUE.
      case (2)
        if (tictac(1, 2) .EQ. " ") check_play = .TRUE.
      case (3)
        if (tictac(1, 3) .EQ. " ") check_play = .TRUE.
      case (4)
        if (tictac(2, 1) .EQ. " ") check_play = .TRUE.
      case (5)
        if (tictac(2, 2) .EQ. " ") check_play = .TRUE.
      case (6)
        if (tictac(2, 3) .EQ. " ") check_play = .TRUE.
      case (7)
        if (tictac(3, 1) .EQ. " ") check_play = .TRUE.
      case (8)
        if (tictac(3, 2) .EQ. " ") check_play = .TRUE.
      case (9)
        if (tictac(3, 3) .EQ. " ") check_play = .TRUE.
      end select

      return
      end
