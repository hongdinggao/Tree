submodule(rbtree) exec
   implicit none
   contains
     module procedure init_node
        implicit none
        newnode%va = va
        newnode%color = RED
        newnode%left => null()
        newnode%right => null()
        newnode%parent => null()
     end procedure init_node

     module procedure init_tree
        implicit none
        tree%root => null()
     end procedure init_tree



     module procedure getcolor
        implicit none
        if (.not.associated(thenode)) then 
           getcolor = BLACK
        else

        getcolor = thenode%color
        end if
     end procedure getcolor

     module procedure setcolor
        implicit none
        if (.not.associated(thenode)) return
        
        thenode%color = color
     end procedure setcolor

     module procedure rotateleft
        implicit none
        type(node), pointer :: right_child => null()
        right_child => ptr%right
        ptr%right => right_child%left

        if (associated(ptr%right)) ptr%right%parent => ptr

        right_child%parent => ptr%parent

        if (.not.associated(ptr%parent)) then
            tree%root => right_child
        else if (associated(ptr, ptr%parent%left)) then   ! to compare 2 pointers in fortran
            ptr%parent%left => right_child
        else 
            ptr%parent%right => right_child
        end if

        right_child%left => ptr
        ptr%parent => right_child
     end procedure rotateleft


     module procedure rotateright
        implicit none
        type(node), pointer :: left_child => null()
        left_child => ptr%left
        ptr%left => left_child%right

        if (associated(ptr%left)) ptr%left%parent => ptr

        left_child%parent => ptr%parent

        if (.not.associated(ptr%parent)) then
            tree%root => left_child
        else if (associated(ptr, ptr%parent%left)) then   ! to compare 2 pointers in fortran
            ptr%parent%left => left_child
        else 
            ptr%parent%right => left_child
        end if

        left_child%right => ptr
        ptr%parent => left_child
     end procedure rotateright


     module procedure fixinsert
        implicit none
        type(node), pointer :: parent => null()
        type(node), pointer :: grandparent => null()
        do while (.not.associated(ptr, tree%root) .and. getcolor(ptr) == RED .and. getcolor(ptr%parent) == RED)
            parent => ptr%parent 
            grandparent => parent%parent
            if (associated(parent, grandparent%left)) then
                block
                  type(node), pointer :: uncle
                  uncle => grandparent%right
                  if (getcolor(uncle) == RED) then
                     call setcolor(uncle, BLACK)
                     call setcolor(parent, BLACK)
                     call setcolor(grandparent, RED)
                     ptr => grandparent
                  else
                     if (associated(ptr, parent%right)) then
                        ptr => parent
                        call rotateleft(tree, parent)
                     end if
                     parent%color = BLACK
                     grandparent%color = RED
                     call rotateright(tree, grandparent)
                  end if
                end block
            else 
                block
                  type(node), pointer :: uncle
                  uncle => grandparent%left 
                  if (getcolor(uncle) == RED) then
                     call setcolor(uncle, BLACK)
                     call setcolor(parent, BLACK)
                     call setcolor(grandparent, RED)
                     ptr => grandparent
                  else
                     if (associated(ptr, parent%left)) then
                        ptr => parent
                        call rotateright(tree, parent)
                     end if
                     parent%color = BLACK
                     grandparent%color = RED
                     call rotateleft(tree, grandparent)
                  end if
                end block
            end if
        end do
        call setcolor(tree%root, BLACK)
     end procedure fixinsert


     module procedure addnode
        implicit none
        if (.not.associated(root)) then 
           root => newnode
        else if (newnode%va < root%va) then
           call addnode(root%left, newnode)
           root%left%parent => root
        else
           call addnode(root%right, newnode)
           root%right%parent => root
        end if
     end procedure addnode






     module procedure addtree
        implicit none
        type(node), pointer :: newnode
        allocate(newnode)
        call init_node(newnode, i)
        call addnode(this%root, newnode)
        call fixinsert(this, newnode)
     end procedure addtree


     module procedure preorderBST
        implicit none
        if (.not.associated(ptr)) return

        write(*, '(i0, 1x, i0)') ptr%va, ptr%color
        call preorderBST(ptr%left)
        call preorderBST(ptr%right)
     end procedure preorderBST

     module procedure preorder
        implicit none
        call preorderBST(this%root)
        write(*,'(a)') "--------"
     end procedure preorder



     module procedure inorderBST
        implicit none
        if (.not.associated(ptr)) return

        call inorderBST(ptr%left)
        write(*, '(i0, 1x, i0)') ptr%va, ptr%color
        call inorderBST(ptr%right)
     end procedure inorderBST

     module procedure inorder
        implicit none
        call inorderBST(this%root)
        write(*,'(a)') "--------"
     end procedure inorder
end submodule exec




































