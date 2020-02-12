module mod_rowcol
   implicit none
   private
   type, public :: rowcol
      integer :: row = 0
      integer :: col = 0
   end type rowcol
end module mod_rowcol
