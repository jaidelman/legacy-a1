program luc

  ! Declare variables
  implicit integer (a-z)
  integer, dimension(0:7, 0:15) :: k
  integer, dimension(0:7,0:7,0:1) :: m
  integer, dimension(0:127) :: key, message
  integer, dimension(0:31) :: kb, mb
  equivalence (k(0,0),key(1)),(m(0,0,0),message(1)) ! Set equivilance for the key and message

  ! Prompt for input and read input
  write(*,1008)
  read(*,1004) (kb(i),i=0,31)
  write(*,1009)
  read(*,1006) (mb(i),i=0,31)

  ! Expand the message and key into bytes
  call expand(message,mb,32)
  call expand(key,kb,32)

  ! Encrypt message
  write(*, 1000)
  d=0
  call lucifer(d,k,m)

  ! Print cipher text
  call compress(message,mb,32)
  write(*,1002) (mb(i), i=0,31)
  call expand(message,mb,32)

  ! Decrypt message
  write(*, 1001)
  d=1
  call lucifer(d,k,m)

  ! Compress the message and key back to text
  call compress(message,mb,32)
  call compress(key,kb,32)

  ! Write decrypted key and message
  write(*,1003)
  write(*,1007) (kb(i),i=0,31)
  write(*,1005)
  write(*,1007) (mb(i),i=0,31)

  ! Format's for output
  1000 format(' Encrypting... ')
  1001 format(' Decrypting... ')
  1002 format(' The cipher is: '/1x,32z1.1)
  1003 format(' Your key was:  ')
  1004 format(32z1.1)
  1005 format(' Your message was: ')
  1006 format(32z1.1)
  1007 format(1x,32z1.1)
  1008 format(' Please enter your key: ')
  1009 format(' Please enter your message: ')
end

! Applies the lucifer encrypting/decrypting algorithm
subroutine lucifer(d,k,m)

  ! Declare variables
  implicit integer(a-z)
  integer, intent(in) :: d
  integer, dimension(0:7,0:7,0:1), intent(inout) ::  m
  integer, dimension(0:7,0:15), intent(in) :: k
  integer, dimension(0:7) :: o = (/7,6,2,1,5,0,3,4/) ! diffusion pattern
  integer, dimension(0:7, 0:7) :: sw
  integer, dimension(0:7) :: tr, pr = (/2,5,4,0,3,1,7,6/) ! pr is inverse of fixed permutation
  integer, dimension(0:1) :: c
  integer, dimension(0:15) :: s0 = (/12,15,7,10,14,13,11,0,2,6,3,1,9,4,5,8/), &
   s1 = (/7,2,14,9,3,11,0,4,12,13,1,10,6,15,8,5/) ! S-box permutations
  integer :: h0=0, h1=1, kc=0
  equivalence (c(0),h),(c(1),l)

  ! If we are decrypting, kc = 8
  if (d == 1) kc=8
  do ii=1,16
    if (d == 1) kc=mod(kc+1,16) ! If we are decrypting, update kc
    ks=kc
    do jj=0,7
      l=0
      h=0
      do kk=0,3
        l=l*2+m(7-kk,jj,h1)
      end do
      do kk=4,7
        h=h*2+m(7-kk,jj,h1)
      end do
      v=(s0(l)+16*s1(h))*(1-k(jj,ks))+(s0(h)+16*s1(l))*k(jj,ks)
      do kk=0,7
        tr(kk)=mod(v,2)
        v=v/2
      end do
      do kk=0,7
        m(kk,mod(o(kk)+jj,8),h0)=mod(k(pr(kk),kc)+tr(pr(kk)) &
        + m(kk,mod(o(kk)+jj,8),h0),2)
      end do
      if (jj < 7 .or. d == 1) kc=mod(kc+1,16)
    end do
    jjj=h0
    h0=h1
    h1=jjj
  end do
  do jj=0,7
    do kk=0,7
      sw(kk,jj)=m(kk,jj,0)
      m(kk,jj,0)=m(kk,jj,1)
      m(kk,jj,1)=sw(kk,jj)
    end do
  end do
  return
end

! Expands a string
subroutine expand(a,b,l)
  implicit integer (a-z)
  integer, dimension(0:*), intent(out) :: a
  integer, dimension(0:*), intent(in) :: b
  integer, intent(in) :: l
  do i=0,l-1
    v=b(i)
      do j=0,3
        a((3-j)+i*4)=mod(v,2)
        v=v/2
      end do
  end do
  return
end

! Compresses a string
subroutine compress(a,b,l)
  implicit integer (a-z)
  integer, dimension(0:*), intent(in) :: a
  integer, dimension(0:*), intent(out) :: b
  integer, intent(in) :: l
  do i=0,l-1
    v=0
      do j=0,3
        v=v*2+mod(a(j+i*4),2)
      end do
    b(i)=v
  end do
  return
end
