submodule(rbtree) exec
   implicit none
   contains
     module procedure init_node
        implicit none
        !allocate(newnode%key, source = key)
        select type (key)
        type is (integer)
           ! OK
        type is (real)
           ! OK
        type is (character(len=*))
           if (len_trim(key) < 1) error stop "Key cannot be blank"
        class default
           error stop "Unknown type used"
        end select
        newnode%key = key
        allocate(newnode%value, source=value)
        newnode%color = RED    ! new node starts with RED
        newnode%left => null()
        newnode%right => null()
        newnode%parent => null()
     end procedure init_node


     ! tree constructor (do not use this)
     module procedure init_tree
        implicit none
        tree%root => null()
     end procedure init_tree


!     module procedure keys_equal
!        implicit none
!        if (same_type_as(k1, k2)) then
!           select type (k1)
!           type is (integer)
!                 select type (k2)
!                 type is (integer)
!                    keys_equal = k1 == k2
!                 end select
!           type is (real)
!                 select type (k2)
!                 type is (real)
!                    keys_equal = k1 == k2
!                 end select
!           type is (character(len=*))
!                 select type (k2)
!                 type is (character(len=*))
!                    keys_equal = k1 == k2
!                 end select
!           class default
!                 keys_equal = .false.
!           end select
!        end if
!     end procedure keys_equal



     module procedure keys_less
        implicit none
        keys_less = .false.
        if (same_type_as(k1, k2)) then
           select type (k1)
           type is (integer)
                 select type (k2)
                 type is (integer)
                    keys_less = k1 < k2
                 end select
           type is (real)
                 select type (k2)
                 type is (real)
                    keys_less = k1 < k2
                 end select
           type is (character(len=*))
                 select type (k2)
                 type is (character(len=*))
                    keys_less = k1 < k2
                 end select
           !class default
           !      keys_less = .false.
           end select
        end if
     end procedure keys_less



     module procedure keys_greater
        implicit none
        keys_greater = .false.
        if (same_type_as(k1, k2)) then
           select type (k1)
           type is (integer)
                 select type (k2)
                 type is (integer)
                    keys_greater = k1 > K2
                 end select
           type is (real)
                 select type (k2)
                 type is (real)
                    keys_greater = k1 > k2
                 end select
           type is (character(len=*))
                 select type (k2)
                 type is (character(len=*))
                    keys_greater = k1 > k2
                 end select
           !class default
           !      keys_greater = .false.
           end select
        end if
     end procedure keys_greater





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
          !deallocate(ptr)
          if (associated(ptr%value)) deallocate(ptr%value)
          nullify(ptr%value)
          call setcolor(tree%root, BLACK)
       end if
    end procedure fixdelete



     module procedure addnode
        implicit none
        if (.not.associated(root)) then 
           root => newnode
        else if (newnode%key .less. root%key) then  ! overloading <
           call addnode(root%left, newnode)
           root%left%parent => root
        else
           call addnode(root%right, newnode)
           root%right%parent => root
        end if
     end procedure addnode

     module procedure addtree
        implicit none
        type(node), pointer :: newnode => null()
        allocate(newnode)
        call init_node(newnode, i, value)
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

        if (i .less. root%key) then       ! overloading <
           killnode => deleteBST(root%left, i)
           return
        end if

        if (i .greater. root%key) then    ! overloading >
           killnode => deleteBST(root%right, i)
           return
        end if

        if ((.not.associated(root%left)) .or. (.not.associated(root%right))) then
           killnode => root
           return
        end if

        temp => minValueNode(root%right)
        root%key = temp%key
        killnode => deleteBST(root%right, temp%key)
     end procedure deleteBST

     module procedure deleteValue
        implicit none
        type(node), pointer :: delnode => null()
        delnode => deleteBST(this%root, i)
        call fixdelete(this, delnode)
     end procedure deleteValue


     ! finalizer
     module procedure clean_tree
        implicit none
        if (associated(this%root)) then
           call kill_node(this%root)
           deallocate(this%root)
        end if
     end procedure clean_tree

     module procedure kill_node
        implicit none
        if (associated(thenode%left)) then
           call kill_node(thenode%left)
           deallocate(thenode%left)
        end if
        if (associated(thenode%right)) then
           call kill_node(thenode%right)
           deallocate(thenode%right)
        end if
     end procedure kill_node




     !========== print ============ 
     module procedure preorderBST
        implicit none
        if (.not.associated(ptr)) return
        
        select type (tokey => ptr%key)
        type is (integer)
           select type (tovalue => ptr%value)
           type is (real)
               write(*, '(i0, 1x, i0, 1x, f8.3)') tokey, ptr%color, tovalue
           end select
        type is (real)
           select type (tovalue => ptr%value)
           type is (real)
               write(*, '(f8.3, 1x, i0, 1x, f8.3)') tokey, ptr%color, tovalue
           end select
        type is (character(len=*))
           select type (tovalue => ptr%value)
           type is (real)
               write(*, '(a, 1x, i0, 1x, f8.3)') tokey, ptr%color, tovalue
           end select
        class default
           write(*,'(a)') "no type matched"
        end select
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
        select type (tokey => ptr%key)
        type is (integer)
           select type (tovalue => ptr%value)
           type is (real)
               write(*, '(i0, 1x, i0, 1x, f8.3)') tokey, ptr%color, tovalue
           end select
        type is (real)
           select type (tovalue => ptr%value)
           type is (real)
               write(*, '(f8.3, 1x, i0, 1x, f8.3)') tokey, ptr%color, tovalue
           end select
        type is (character(len=*))
           select type (tovalue => ptr%value)
           type is (real)
               write(*, '(a, 1x, i0, 1x, f8.3)') tokey, ptr%color, tovalue
           end select
        class default
           write(*,'(a)') "no type matched"
        end select
        call inorderBST(ptr%right)
     end procedure inorderBST

     module procedure inorder
        implicit none
        call inorderBST(this%root)
        write(*,'(a)') "--------"
     end procedure inorder
end submodule exec




































