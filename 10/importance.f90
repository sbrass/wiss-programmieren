module importance
  use iso_fortran_env, only: r64 => REAL64, i64 => INT64, OUTPUT_UNIT

  implicit none

  private

  public :: simple_sampling, importance_sampling, markov_sampling
contains
  function simple_sampling (func, beta, a, b, n) result (int)
    interface
       function func (xi, beta) result (f)
         import :: r64
         real(r64), intent(in) :: xi
         real(r64), intent(in) :: beta
         real(r64) :: f
       end function func
    end interface
    real(r64), intent(in) :: beta
    real(r64), intent(in) :: a, b
    integer(i64), intent(in) :: n
    real(r64) :: int
    integer(i64) :: i
    real(r64) :: r, x
    int = 0.
    do i = 1, n
       call random_number (r)
       x = (b - a) * r
       int = int + func (x, beta)
    end do
    int = (b - a) * int / n
  end function simple_sampling

  function importance_sampling (func, beta, n) result (int)
    interface
       function func (xi, beta) result (f)
         import :: r64
         real(r64), intent(in) :: xi
         real(r64), intent(in) :: beta
         real(r64) :: f
       end function func
    end interface
    real(r64), intent(in) :: beta
    integer(i64), intent(in) :: n
    real(r64) :: int
    integer(i64) :: i
    real(r64) :: r, x
    int = 0
    do i = 1, n
       call random_number (r)
       !! r = P(x) = 1 - exp(-β x)
       !! r = exp(-β x) ↔ x = - log (1 - r) / β
       !! x' = 1 / (β (1 - r))
       x = - log (1 - r) / beta
       int = int + func (x, beta) / (beta * (1 - r)) !! Apply Jacobian Δ = 1 / (β·(1- r))
    end do
    int = int / n
  end function importance_sampling

  function markov_sampling (func, beta, a, b, n) result (int)
    interface
       function func (xi, beta) result (f)
         import :: r64
         real(r64), intent(in) :: xi
         real(r64), intent(in) :: beta
         real(r64) :: f
       end function func
    end interface
    real(r64), intent(in) :: beta
    real(r64), intent(in) :: a, b
    integer(i64), intent(in) :: n
    real(r64) :: int
    integer(i64) :: i
    real(r64) :: xi, xj, r, p, priori, priorj
    int = 0.
    call random_number (r)
    xi = (b - a) * r
    do i = 1, n
       call random_number (r)
       !! Prior (specific).
       !! p(x) = 1 / (1 + x)^2.
       !! P(x) = ∫_0^∞ p(x') dx' = 1 / (1 + x) = u.
       !! x = 1 / u - 1.
       ! xj = 1 / r - 1
       ! priorj = 1 / (1 + xj)**2
       ! priori = 1 / (1 + xi)**2
       !! Prior (constant).
       xj = (b - a) * r
       priori = 1.
       priorj = 1.
       p = min (1._r64, pdf (xj, beta) / pdf (xi, beta) * priori / priorj)
       call random_number (r)
       !! Akzeptieren.
       if (r <= p) then
          xi = xj
       end if
       !! <f(x)> = ∫ f(x) 1(x) dx
       !! → ∫ f(x) / p(x) (p(x) dx) = ∫ f(x) / p(x) dP(x)
       !! Wir ziehen x aus einer Verteilung P(x) (mit der Dichte p(x)),
       !! für jedes x müssen den Integranden mit der Dichte p(x) reskalieren.
       int = int + func (xi, beta) / pdf (xi, beta)
    end do
    int = int / n
  contains
    function pdf (x, a) result (p)
      real(r64), intent(in) :: x
      real(r64), intent(in) :: a
      real(r64) :: p
      !! Sicherheitsvorkehrung, gegen p(x) = 0.
      p = max (a * exp (-a * x), tiny(1._r64))
    end function pdf
  end function markov_sampling
end module importance
