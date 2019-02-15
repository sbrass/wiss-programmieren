module polynom
  implicit none

  private

  type :: polynom_t
     private
     integer :: degree
     !! Coefficients
     real, dimension(:), allocatable :: c
   contains
     procedure :: write => polynom_write
     procedure :: lc => polynom_get_leading_coefficient
     procedure :: deg => polynom_get_degree
     generic :: operator(+) => add
     generic :: operator(-) => subtract
     generic :: operator(*) => multiply
     procedure, private :: add => polynom_add
     procedure, private :: subtract => polynom_subtract
     procedure, private :: multiply => polynom_multiply
     procedure, private :: reduce => polynom_reduce
  end type polynom_t

  interface polynom_t
     module procedure polynom_init_coefficients
  end interface polynom_t

  real, parameter :: EPS_REDUCE = 0.001

  public :: polynom_t, polynom_single_coeff
contains
  type(polynom_t) function polynom_init_coefficients (c) result(p)
    real, dimension(:), intent(in) :: c
    p%degree = size(c) - 1
    p%c = c
  end function polynom_init_coefficients

  type(polynom_t) function polynom_single_coeff (i, c) result(p)
    integer, intent(in) :: i
    real, intent(in) :: c
    p%degree = i
    allocate (p%c(i + 1), source = 0.)
    p%c(i + 1) = c
  end function polynom_single_coeff

  subroutine polynom_write (p, u)
    class(polynom_t), intent(in) :: p
    integer, intent(in) :: u
    integer :: i
    do i = p%degree, 0, -1
       write (u, '(A,F6.3,A,I2,A)', advance='no') " + (", p%c(i + 1), ")* x^(", i, ")"
    end do
    write (u, *)
  end subroutine polynom_write

  real function polynom_get_leading_coefficient (p) result (lc)
    class(polynom_t), intent(in) :: p
    lc = p%c(p%degree + 1)
  end function polynom_get_leading_coefficient

  integer function polynom_get_degree (p) result (deg)
    class(polynom_t), intent(in) :: p
    deg = p%degree
  end function polynom_get_degree

  type(polynom_t) function polynom_add (p1, p2) result (p)
    class(polynom_t), intent(in) :: p1
    type(polynom_t), intent(in) :: p2
    real, dimension(:), allocatable :: c
    if (p1%deg () >= p2%deg ()) then
       p%degree = p1%deg ()
       c = p1%c
       c(:p2%deg () + 1) = c(:p2%deg () + 1) + p2%c
    else
       p%degree = p2%deg ()
       c = p2%c
       c(:p1%deg () + 1) = c(:p1%deg () + 1) + p1%c
    end if
    p%c = c
    call p%reduce ()
  end function polynom_add

  type(polynom_t) function polynom_subtract (p1, p2) result (p)
    class(polynom_t), intent(in) :: p1
    type(polynom_t), intent(in) :: p2
    real, dimension(:), allocatable :: c
    if (p1%deg () >= p2%deg ()) then
       p%degree = p1%deg ()
       c = p1%c
       c(:p2%deg () + 1) = c(:p2%deg () + 1) - p2%c
    else
       p%degree = p2%deg ()
       c = p2%c
       c(:p1%deg () + 1) = c(:p1%deg () + 1) - p1%c
    end if
    p%c = c
    call p%reduce ()
  end function polynom_subtract

  type(polynom_t) function polynom_multiply (p1, p2) result(p)
    class(polynom_t), intent(in) :: p1
    type(polynom_t), intent(in) :: p2
    integer :: deg, i, j
    real, dimension(:), allocatable :: c
    deg = p1%deg () + p2%deg ()
    allocate (c(deg + 1), source = 0.0)
    do i = 1, p1%deg () + 1
       do j = 1, p2%deg () + 1
          c(i + j - 1) = p1%c(i) * p2%c(j)
       end do
    end do
    p%degree = deg
    p%c = c
    call p%reduce ()
  end function polynom_multiply

  subroutine polynom_reduce (p)
    class(polynom_t), intent(inout) :: p
    integer :: i
    do i = p%deg () + 1, 1, -1
       if (p%c(i) <= EPS_REDUCE) then
          p%degree = p%degree - 1
          p%c = p%c(:i - 1)
          cycle
       end if
       exit
    end do
  end subroutine polynom_reduce

end module polynom
