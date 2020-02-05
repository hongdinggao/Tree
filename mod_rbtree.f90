module rbtree
  implicit none
  public :: init_tree
  private
  integer, parameter :: RED = 0
  integer, parameter :: BLACK = 1
  integer, parameter :: DOUBLE_BLACK = 2

  type, public :: node
     integer(kind=8) :: va
     integer :: color = RED 
     type(node), pointer :: left => null()
     type(node), pointer :: right => null()
     type(node), pointer :: parent => null()
  end type node


  type, public :: thetree
     private
     type(node), pointer :: root => null()
     contains
     generic, public :: assignment(=) => add
     procedure :: add => addtree
     procedure :: preorder
     procedure :: inorder
     procedure :: deleteValue
  end type thetree

  ! user_defined constructor for a tree
  interface thetree 
     module function init_tree()  result(tree)
        type(thetree) :: tree
     end function init_tree
  end interface 



  interface
     ! constructor of node
     module subroutine init_node(newnode, va)
        type(node), pointer, intent(inout) :: newnode
        integer(kind=8), intent(in) :: va 
     end subroutine init_node

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
        integer(kind=8), intent(in) :: i
     end subroutine addtree

     recursive module function minValueNode(thenode) result(minnode)
        type(node), pointer, intent(in) :: thenode
        type(node), pointer :: minnode
     end function minValueNode

     recursive module function deleteBST(root, i) result(killnode)
        type(node), pointer, intent(inout) :: root
        integer(kind=8), intent(in) :: i
        type(node), pointer :: killnode
     end function deleteBST

     module subroutine deleteValue(this, i)
        class(thetree), intent(inout) :: this
        integer(kind=8), intent(in) :: i
     end subroutine deleteValue

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
































