module mod_rowcol
   use, intrinsic :: iso_fortran_env
   implicit none
   private
   type, public :: rowcol
      integer(int64) :: row = 0
      integer(int64) :: col = 0
   end type rowcol
end module mod_rowcol
