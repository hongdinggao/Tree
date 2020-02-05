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

     ! tree constructor
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

     
    module procedure fixdelete
       implicit none
       if (.not.associated(ptr)) return
       if (associated(ptr, tree%root)) then
          tree%root => null()
          return
       end if
       if (getcolor(ptr) == RED .or. getcolor(ptr%left) == RED .or. getcolor(ptr%right) == RED) then
          block
            type(node), pointer :: child => null()
            if (associated(ptr%left)) then
               child => ptr%left
            else
               child => ptr%right
            end if
            if (associated(ptr, ptr%parent%left)) then
               ptr%parent%left => child
               if (associated(child)) then
                  child%parent => ptr%parent
               end if
               call setcolor(child, BLACK)
               deallocate(ptr)
            else
               ptr%parent%right => child
               if (associated(child)) then
                  child%parent => ptr%parent
               end if
               call setcolor(child, BLACK)
               deallocate(ptr)
            end if
          end block
       else
          block
            type(node), pointer :: sibling => null()
            type(node), pointer :: parent => null()
            type(node), pointer :: ptr2 => null()
            ptr2 => ptr
            call setcolor(ptr2, DOUBLE_BLACK)
            do while (.not.associated(ptr2, tree%root) .and. getcolor(ptr2) == DOUBLE_BLACK)
               parent => ptr2%parent
               if (associated(ptr2, parent%left)) then
                  sibling => parent%right
                  if (getcolor(sibling) == RED) then
                     call setcolor(sibling, BLACK)
                     call setcolor(parent, RED)
                     call rotateleft(tree, parent)
                  else
                     if (getcolor(sibling%left) == BLACK .and. getcolor(sibling%right) == BLACK) then
                        call setcolor(sibling, RED)
                        if (getcolor(parent) == RED) then
                           call setcolor(parent, BLACK)
                        else
                           call setcolor(parent, DOUBLE_BLACK)
                        end if
                        ptr2 => parent
                     else
                        if (getcolor(sibling%right) == BLACK) then
                           call setcolor(sibling%left, BLACK)
                           call setcolor(sibling, RED)
                           call rotateright(tree, sibling)
                           sibling => parent%right
                        end if
                        call setcolor(sibling, parent%color)
                        call setcolor(parent, BLACK)
                        call setcolor(sibling%right, BLACK)
                        call rotateleft(tree, parent)
                        exit
                     end if
                  end if
               else
                  sibling => parent%left
                  if (getcolor(sibling) == RED) then
                     call setcolor(sibling, BLACK)
                     call setcolor(parent, RED)
                     call rotateright(tree, parent)
                  else
                     if (getcolor(sibling%left) == BLACK .and. getcolor(sibling%right) == BLACK) then
                        call setcolor(sibling, RED)
                        if (getcolor(parent) == RED) then
                           call setcolor(parent, BLACK)
                        else
                           call setcolor(parent, DOUBLE_BLACK)
                        end if
                        ptr2 => parent
                     else
                        if (getcolor(sibling%left) == BLACK) then
                           call setcolor(sibling%right, BLACK)
                           call setcolor(sibling, RED)
                           call rotateleft(tree, sibling)
                           sibling => parent%left
                        end if 
                        call setcolor(sibling, parent%color)
                        call setcolor(parent, BLACK)
                        call setcolor(sibling%left, BLACK)
                        call rotateright(tree, parent)
                        exit
                     end if
                  end if
               end if
            end do
          end block
          if (associated(ptr, ptr%parent%left)) then
             ptr%parent%left => null()
          else
             ptr%parent%right => null()
          end if
          deallocate(ptr)
          call setcolor(tree%root, BLACK)
       end if
    end procedure fixdelete



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


     module procedure minValueNode
        implicit none
        if (.not.associated(thenode%left)) then
           minnode => thenode
        else
           minnode => minValueNode(thenode%left)
        end if
     end procedure minValueNode


     module procedure deleteBST
        implicit none
        type(node), pointer :: temp => null()
        if (.not.associated(root)) then
           killnode => root
           return
        end if

        if (i < root%va) then
           killnode => deleteBST(root%left, i)
           return
        end if

        if (i > root%va) then
           killnode => deleteBST(root%right, i)
           return
        end if

        if ((.not.associated(root%left)) .or. (.not.associated(root%right))) then
           killnode => root
           return
        end if

        temp => minValueNode(root%right)
        root%va = temp%va
        killnode => deleteBST(root%right, temp%va)
     end procedure deleteBST

     module procedure deleteValue
        implicit none
        type(node), pointer :: delnode => null()
        delnode => deleteBST(this%root, i)
        call fixdelete(this, delnode)
     end procedure deleteValue







     !========== print ============ 
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




































