module list
  implicit none

  private

  type, abstract :: basic_node_t
     class(basic_node_t), pointer :: next => null ()
   contains
     procedure(basic_node_write), deferred :: write
  end type basic_node_t

  abstract interface
     subroutine basic_node_write (node)
       import :: basic_node_t
       class(basic_node_t), intent(in) :: node
     end subroutine basic_node_write
  end interface

  !> FIFO-List.
  type :: list_t
     private
     integer :: size = 0
     class(basic_node_t), pointer :: first => null ()
     class(basic_node_t), pointer :: last => null ()
   contains
     procedure :: push => list_push_node
     procedure :: pop => list_pop_node
     procedure :: front => list_front_node
     procedure :: back => list_back_node
     procedure :: write => list_write
     final :: list_finalize
  end type list_t

  public :: list_t, basic_node_t
contains
  !> Push last element.
  subroutine list_push_node (list, node)
    class(list_t), intent(inout) :: list
    class(basic_node_t), pointer, intent(in) :: node
    if (.not. associated (list%first)) then
       list%first => node
    end if
    if (associated (list%last)) then
       list%last%next => node
    end if
    list%last => node
  end subroutine list_push_node

  !> Remove next element.
  subroutine list_pop_node (list)
    class(list_t), intent(inout) :: list
    class(basic_node_t), pointer :: node
    if (.not. associated (list%first)) return
    node => list%first
    if (associated (node%next)) then
       list%first => node%next
    else
       !! List is empty now.
       list%first => null ()
       list%last => null ()
    end if
    deallocate (node)
  end subroutine list_pop_node

  function list_front_node (list) result (node)
    class(list_t), intent(in) :: list
    class(basic_node_t), pointer :: node
    node => list%first
  end function list_front_node

  function list_back_node (list) result (node)
    class(list_t), intent(in) :: list
    class(basic_node_t), pointer :: node
    node => list%last
  end function list_back_node

  subroutine list_write (list)
    class(list_t), intent(in) :: list
    class(basic_node_t), pointer :: node
    node => list%front ()
    do while (associated (node))
       call node%write ()
       node => node%next
    end do
  end subroutine list_write

  subroutine list_finalize (list)
    type(list_t) :: list
    class(basic_node_t), pointer :: node
    do while (associated (list%first))
       node => list%first
       list%first => list%first%next
       deallocate (node)
    end do
  end subroutine list_finalize
end module list
