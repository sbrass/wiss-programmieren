module divisor
  use polynom

  implicit none

  private

  type, abstract :: polynom_divisor_t
     type(polynom_t) :: q
     type(polynom_t) :: r
   contains
     procedure :: base_write => polynom_divisor_base_write
     procedure(polynom_divisor_write), deferred :: write
     procedure(polynom_divisor_divide), deferred :: divide
     procedure(polynom_divisor_get_quotient), deferred :: quotient
     procedure(polynom_divisor_get_remainder), deferred :: remainder
  end type polynom_divisor_t

  abstract interface
     subroutine polynom_divisor_write (d, u)
       import
       class(polynom_divisor_t), intent(in) :: d
       integer, intent(in) :: u
     end subroutine polynom_divisor_write

     subroutine polynom_divisor_divide (d, a, b)
       import
       class(polynom_divisor_t), intent(out) :: d
       type(polynom_t), intent(in) :: a
       type(polynom_t), intent(in) :: b
     end subroutine polynom_divisor_divide

     type(polynom_t) function polynom_divisor_get_quotient (d) result (p)
       import
       class(polynom_divisor_t), intent(in) :: d
     end function polynom_divisor_get_quotient

     type(polynom_t) function polynom_divisor_get_remainder (d) result (p)
       import
       class(polynom_divisor_t), intent(in) :: d
     end function polynom_divisor_get_remainder
 end interface

  public :: polynom_divisor_t
contains
  subroutine polynom_divisor_base_write (d, name, u)
    class(polynom_divisor_t), intent(in) :: d
    character(len=*), intent(in) :: name
    integer, intent(in) :: u
    write (u, '(A)') name
    write (u, '(A)') "Quotient: "
    call d%q%write (u)
    write (u, '(A)') "Remainder: "
    call d%r%write (u)
  end subroutine polynom_divisor_base_write
end module divisor
