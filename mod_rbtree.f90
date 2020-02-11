module rbtree
  implicit none
  !public :: init_tree
  private
  integer, parameter :: RED = 0
  integer, parameter :: BLACK = 1
  integer, parameter :: DOUBLE_BLACK = 2

  type, public :: node
     class(*), allocatable :: key   ! unlimited polymorphic
     integer :: color = RED 
     type(node), pointer :: left => null()
     type(node), pointer :: right => null()
     type(node), pointer :: parent => null()
  end type node


  type, public :: thetree
     private
     type(node), pointer :: root => null()
     contains
     private
     generic, public :: assignment(=) => add
     !generic, public :: operator(==) => keys_equal
     procedure :: add => addtree
     !procedure :: keys_equal
     procedure, public :: preorder
     procedure, public :: inorder
     procedure, public :: deleteValue
     final :: clean_tree
  end type thetree

  ! user_defined constructor for a tree, only function allowed
  interface thetree 
     module function init_tree()  result(tree)
        type(thetree) :: tree
     end function init_tree
  end interface thetree 


  ! does not work since interface and body are separate in submodule
!  abstract interface
!     module subroutine rotate_fix(tree, ptr)
!        import :: node, thetree
!        type(thetree), intent(inout) :: tree
!        type(node), pointer, intent(inout) :: ptr
!     end subroutine rotate_fix
!  end interface

!  procedure(rotate_fix) :: rotateleft, rotateright, fixinsert, fixdelete


!  interface operator(==)
!     pure module function keys_equal(k1, k2)
!        class(*), intent(in) :: k1, k2
!        logical :: keys_equal
!     end function keys_equal
!  end interface operator(==)

  interface operator(.less.)    ! ifort not allow overloading <
     module procedure :: keys_less
  end interface operator(.less.)


  interface operator(.greater.) ! ifort not allow overloading > 
     module procedure :: keys_greater
  end interface operator(.greater.)


    
  interface
     pure module function keys_less(k1, k2)
        class(*), intent(in) :: k1, k2
        logical :: keys_less
     end function keys_less




     pure module function keys_greater(k1, k2)
        class(*), intent(in) :: k1, k2
        logical :: keys_greater
     end function keys_greater





     ! constructor of node
     module subroutine init_node(newnode, key)
        type(node), pointer, intent(inout) :: newnode
        class(*), intent(in) :: key
     end subroutine init_node


!     module function init_node(key) result(newnode)
!        type(node), pointer :: newnode
!        class(*), intent(in) :: key
!     end function init_node

     module function getcolor(thenode)
        type(node), pointer, intent(in) :: thenode
        integer :: getcolor
     end function getcolor

     module subroutine setcolor(thenode, color)
        type(node), pointer, intent(inout) :: thenode
        integer, intent(in) :: color
     end subroutine setcolor

     module subroutine rotateleft(tree, ptr)
        type(thetree), intent(inout) :: tree
        type(node), pointer, intent(inout) :: ptr
     end subroutine rotateleft
     
     module subroutine rotateright(tree, ptr)
        type(thetree), intent(inout) :: tree
        type(node), pointer, intent(inout) :: ptr
     end subroutine rotateright
     
     module subroutine fixinsert(tree, ptr)
        type(thetree), intent(inout) :: tree
        type(node), pointer, intent(inout) :: ptr
     end subroutine fixinsert

     module subroutine fixdelete(tree, ptr)
        type(thetree), intent(inout) :: tree
        type(node), pointer, intent(inout) :: ptr
     end subroutine fixdelete


     recursive module subroutine addnode(root, newnode) 
        type(node), pointer, intent(inout) :: root
        type(node), pointer, intent(in) :: newnode
     end subroutine addnode

     module subroutine addtree(this, i)
        class(thetree), intent(inout) :: this
        class(*), intent(in) :: i
     end subroutine addtree

     recursive module function minValueNode(thenode) result(minnode)
        type(node), pointer, intent(in) :: thenode
        type(node), pointer :: minnode
     end function minValueNode

     recursive module function deleteBST(root, i) result(killnode)
        type(node), pointer, intent(inout) :: root
        class(*), intent(in) :: i
        type(node), pointer :: killnode
     end function deleteBST

     module subroutine deleteValue(this, i)
        class(thetree), intent(inout) :: this
        class(*), intent(in) :: i
     end subroutine deleteValue

     ! destroy the tree
     module subroutine clean_tree(this)
         type(thetree), intent(inout) :: this
     end subroutine clean_tree

     ! destory a node
     recursive module subroutine kill_node(thenode)
         type(node), intent(inout) :: thenode
     end subroutine kill_node


     recursive module subroutine preorderBST(ptr)
        type(node), pointer, intent(in) :: ptr
     end subroutine preorderBST

     module subroutine preorder(this)
        class(thetree), intent(inout) :: this
     end subroutine preorder


     recursive module subroutine inorderBST(ptr)
        type(node), pointer, intent(in) :: ptr
     end subroutine inorderBST

     module subroutine inorder(this)
        class(thetree), intent(inout) :: this
     end subroutine inorder
  end interface
end module rbtree
































