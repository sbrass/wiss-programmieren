module cluster
  use iso_fortran_env, only: ERROR_UNIT
  use list, only: list_t, basic_node_t

  implicit none

  private

  type, extends(basic_node_t) :: cluster_node_t
     integer, dimension(2) :: c
   contains
     procedure :: write => cluster_node_write
  end type cluster_node_t

  type :: grid_t
     private
     integer :: size = 0
     logical, dimension(:, :), allocatable :: site
     integer, dimension(:, :), allocatable :: cluster
     integer, dimension(:), allocatable :: cluster_size
     logical :: percolation = .false.
   contains
     procedure :: init => grid_init
     procedure :: print_grid => grid_pretty_print
     procedure :: print_cluster => grid_pretty_print_cluster
     procedure :: search_cluster => grid_search_cluster
     procedure :: is_percolated => grid_is_percolated
     procedure :: get_ratio_max_cluster => grid_get_ratio_max_cluster
  end type grid_t

  public :: grid_t
contains
  subroutine cluster_node_write (node)
    class(cluster_node_t), intent(in) :: node
    print *, node%c
  end subroutine cluster_node_write

  subroutine grid_init (grid, p, l)
    class(grid_t), intent(out) :: grid
    real, intent(in) :: p
    integer, intent(in) :: l
    real, dimension(:, :), allocatable :: p_site
    grid%size = l
    allocate (grid%site (l, l), source = .false.)
    allocate (grid%cluster (l, l), source = 0)
    allocate (p_site (l, l), source = 0.)
    call random_number (p_site)
    grid%site = (p_site < p)
  end subroutine grid_init

  subroutine grid_pretty_print (grid)
    class(grid_t), intent(in) :: grid
    integer :: j
    do j = 1, grid%size
       write (ERROR_UNIT, *) grid%site(:, j)
    end do
  end subroutine grid_pretty_print

  subroutine grid_search_cluster (grid)
    class(grid_t), intent(inout) :: grid
    type(list_t) :: list
    class(basic_node_t), pointer :: site
    integer :: i, j, label
    label = 0
    do while (any (grid%site))
       !! Find first occupied site.
       outer: do j = 1, grid%size
          do i = 1, grid%size
             if (grid%site(i, j)) then
                call add_site (list, grid, [i, j])
                exit outer
             end if
          end do
       end do outer
       label = label + 1
       !! Start cluster search, beginning with label 1.
       do while (associated (list%front ()))
          site => list%front ()
          select type (site)
          type is (cluster_node_t)
             !! Already visited?
             if (left (grid, site%c)) then
                call add_site (list, grid, [site%c(1) - 1, site%c(2)])
             end if
             if (right (grid, site%c)) then
                call add_site (list, grid, [site%c(1) + 1, site%c(2)])
             end if
             if (above (grid, site%c)) then
                call add_site (list, grid, [site%c(1), site%c(2) - 1])
             end if
             if (below (grid, site%c)) then
                call add_site (list, grid, [site%c(1), site%c(2) + 1])
             end if
             grid%cluster(site%c(1), site%c(2)) = label
          end select
          call list%pop () !! Remove current site from list.
          ! print *, "|--------------------------------------"
          ! call list%write ()
       end do
       !! Test on percolation.
       !! Test whether the current cluster hits left and right side of the grid, or top/bottom.
       grid%percolation = grid%percolation &
            .or. (any (grid%cluster(1, :) == label) .and. any (grid%cluster(grid%size, :) == label)) &
            .or. (any (grid%cluster(:, 1) == label) .and. any (grid%cluster(:, grid%size) == label))
    end do
    !! Count cluster sizes.
    allocate (grid%cluster_size (label))
    do i = 1, label
       grid%cluster_size (i) = count (grid%cluster == i)
    end do
    ! write (ERROR_UNIT, *) maxval (grid%cluster_size)
  contains
    function right (grid, c) result (flag)
      type(grid_t), intent(in) :: grid
      integer, dimension(2), intent(in) :: c
      logical :: flag
      if (c(1) + 1 <= grid%size) then
         flag = grid%site(c(1) + 1, c(2))
      else
         flag = .false.
      end if
    end function right

    function left (grid, c) result (flag)
      type(grid_t), intent(in) :: grid
      integer, dimension(2), intent(in) :: c
      logical :: flag
      if (c(1) - 1 >= 1) then
         flag = grid%site(c(1) - 1, c(2))
      else
         flag = .false.
      end if
    end function left

    function above (grid, c) result (flag)
      type(grid_t), intent(in) :: grid
      integer, dimension(2), intent(in) :: c
      logical :: flag
      if (c(2) - 1 >= 1) then
         flag = grid%site(c(1), c(2) - 1)
      else
         flag = .false.
      end if
    end function above

    function below (grid, c) result (flag)
      type(grid_t), intent(in) :: grid
      integer, dimension(2), intent(in) :: c
      logical :: flag
      if (c(2) + 1 <= grid%size) then
         flag = grid%site(c(1), c(2) + 1)
      else
         flag = .false.
      end if
    end function below

    subroutine add_site (list, grid, c)
      type(list_t), intent(inout) :: list
      type(grid_t), intent(inout) :: grid
      integer, dimension(2), intent(in)  :: c
      class(basic_node_t), pointer :: site
      allocate (cluster_node_t :: site)
      select type (site)
      type is (cluster_node_t)
         site%c = c
      end select
      call list%push (site)
      grid%site(c(1), c(2)) = .false.
    end subroutine add_site
  end subroutine grid_search_cluster

  subroutine grid_pretty_print_cluster (grid)
    class(grid_t), intent(in) :: grid
    integer :: j
    do j = 1, grid%size
       print "(100(1X,I4))", grid%cluster(:, j)
    end do
  end subroutine grid_pretty_print_cluster

  logical function grid_is_percolated (grid) result (flag)
    class(grid_t), intent(in) :: grid
    flag = grid%percolation
  end function grid_is_percolated

  real function grid_get_ratio_max_cluster (grid) result (m_inf)
    class(grid_t), intent(in) :: grid
    if (size (grid%cluster_size) == 0) then
       m_inf = 0
    else
       m_inf = real (maxval (grid%cluster_size)) / real (grid%size**2)
    end if
  end function grid_get_ratio_max_cluster
end module cluster
