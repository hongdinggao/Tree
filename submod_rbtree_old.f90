submodule(rbtree) exec
   implicit none
   contains
     module procedure init_node
        implicit none
        !allocate(newnode)
        newnode%va = i
        newnode%color = RED
        newnode%left => null()
        newnode%right => null()
        newnode%parent => null()
     end procedure init_node

     module procedure getcolor
        implicit none
        if (.not.associated(thenode)) getcolor = BLACK

        getcolor = thenode%color
     end procedure getcolor

     module procedure setcolor
        implicit none
        if (.not.associated(thenode)) return
        
        thenode%color = color
     end procedure setcolor

     module procedure rotateleft
        implicit none
        type(node), pointer :: right_child
        right_child => ptr%right
        ptr%right => right_child%left

        if (associated(ptr%right)) ptr%right%parent => ptr

        right_child%parent => ptr%parent

        if (.not.associated(ptr%parent)) then
            this%root => right_child
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
        type(node), pointer :: left_child
        left_child => ptr%left
        ptr%left => left_child%right

        if (associated(ptr%left)) ptr%left%parent => ptr

        left_child%parent => ptr%parent

        if (.not.associated(ptr%parent)) then
            this%root => left_child
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

        do while (.not.associated(ptr, this%root) .and. getcolor(ptr) == RED .and. getcolor(ptr%parent) == RED)
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
                        call rotateleft(parent)
                        ptr => parent
                        parent => ptr%parent
                     end if
                     call rotateright(grandparent)
                     ! switch color with temp variable
                     ! a = a + b; b = a - b; a = a - b
                     parent%color = parent%color + grandparent%color
                     grandparent%color = parent%color - grandparent%color
                     parent%color = parent%color - grandparent%color
                     ! finish switch
                     ptr => parent
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
                        call rotateright(parent)
                        ptr => parent
                        parent => ptr%parent
                     end if
                     call rotateleft(grandparent)
                     ! switch color with temp variable
                     ! a = a + b; b = a - b; a = a - b
                     parent%color = parent%color + grandparent%color
                     grandparent%color = parent%color - grandparent%color
                     parent%color = parent%color - grandparent%color
                     ! finish switch
                     ptr => parent
                  end if
                end block
            end if
        end do
        call setcolor(this%root, BLACK)
     end procedure fixinsert


     module procedure addnode
        implicit none
        if (i < root%va) then
           if (associated(root%left)) then
              call addnode(root%left, i)
           else
              allocate(root%left)
              root%left%va = i
              root%left%color = RED
           end if
        else
           if (associated(root%right)) then
              call addnode(root%right, i)
           else
              allocate(root%right)
              root%right%va = i
              root%right%color = RED
           end if
        end if
     end procedure addnode


     module procedure addtree
        implicit none
        if (.not.associated(this%root)) then
           allocate(this%root)
           this%root%va = i
           this%root%color = RED
        else
           call addnode(this%root, i)
        end if
        
        !call fixinsert(this, newnode)
     end procedure addtree


     module procedure preorderBST
        implicit none
        if (.not.associated(ptr)) return

        write(*, '(i0, 1x)') ptr%va, ptr%color
        call preorderBST(ptr%left)
        call preorderBST(ptr%right)
     end procedure preorderBST

     module procedure preorder
        implicit none
        call preorderBST(this%root)
        write(*,*) "--------"
     end procedure preorder



     module procedure inorderBST
        implicit none
        if (.not.associated(ptr)) return

        call inorderBST(ptr%left)
        write(*, '(i0, 1x)') ptr%va, ptr%color
        call inorderBST(ptr%right)
     end procedure inorderBST

     module procedure inorder
        implicit none
        call inorderBST(this%root)
        write(*,*) "--------"
     end procedure inorder
end submodule exec




































