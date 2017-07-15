defmodule Test do

  @n 20

  def s(0), do: 0
  def s(n) do
    Enum.sum(for k <- 1..(div @n, 2), do: p n, k) + s(n - 1)
  end



  def p(n, i) do
    if Enum.member?(all_sums(i), n) do 1 else 0 end
  end

  def all_sums(1), do: primes
  def all_sums(i) do
    Enum.uniq(
      for x <- primes,
          a <- all_sums(i - 1),
          x + a <= @n,
      do: x + a
    )
  end

  def primes, do: Enum.to_list(2..@n) |> sieve

  def sieve([]),     do: []
  def sieve([p|ps]), do: [ p | sieve(for x <- ps, rem(x, p) > 0, do: x) ]
end
