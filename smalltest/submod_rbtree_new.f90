submodule(rbtree) exec
   implicit none
   contains

     module procedure addnode
        implicit none
        if (i < root%va) then
           if (associated(root%left)) then
              call addnode(root%left, i)
           else
              allocate(root%left)
              root%left%va = i
              !root%left%color = RED
           end if
        else
           if (associated(root%right)) then
              call addnode(root%right, i)
           else
              allocate(root%right)
              root%right%va = i
              !root%right%color = RED
           end if
        end if
     end procedure addnode


     module procedure addtree
        implicit none
        if (.not.associated(this%root)) then
           allocate(this%root)
           this%root%va = i
           !this%root%color = RED
        else
           call addnode(this%root, i)
        end if
        print*, "adding finish"
        print*, this%root%va
        !call fixinsert(this, newnode)
     end procedure addtree


end submodule exec




































