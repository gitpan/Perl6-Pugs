# Snowing on Pugs.
# Run this on a screen at least 72 wide by 30 deep.
# Enjoy! -- asavige aka mad golfer

sub sleepy{for 1..5500 {;}}
sub C{system($?OS eq 'MSWin32'??'ClS'!!'clear')}
my$u;my$g='#';my$U;my$s='.';
for (71,0,71,0,2,13,56,0,2,16,53,0,4,5,3,7,52,0,4,5,5
,6,51,0,4,5,6,6,50,0,4,5,6,6,50,0,4,5,6,6,50,0,4,5,6
,5,2,7,3,6,6,12,4,6,1,1,3,0,4,5,5,5,5,5,4,5,5,4,2,4,
6,3,4,2,3,0,4,14,6,5,4,5,4,4,4,4,4,3,6,1,3,0,4,11,9,
5,4,5,3,4,6,4,3,4,9,0,4,7,13,5,4,5,3,4,6,4,4,4,8,0,4
,5,15,5,4,5,3,4,6,4,5,5,6,0,4,5,15,5,4,5,4,4,4,4,8,5
,4,0,4,5,15,5,4,5,5,4,2,4,10,5,3,0,4,5,15,5,3,6,7,7,
6,1,5,5,2,0,2,10,12,14,6,1,13,2,2,6,3,0,2,10,13,5,3,
6,4,12,3,1,1,6,5,0,42,14,15,0,41,4,7,4,15,0,41,3,9,3
,15,0,41,3,9,3,15,0,42,13,16,0,44,9,18,0,71,0,71,0)
->$p {$u~=($g=$g~^"\r")x$p~"\n"x!$p;
     $U~=($s=$s~^"\r")x$p~"\n"x!$p}
my@v=split("\n",$u).map:{$_~"\n"};@v.pop;
my@x=(1..28).map:{join("",(1..71).map():{my$r=rand();$r < 0.2??'.'!!' '})~"\n"};
my@y=@x;
for -28..0 ->$i {
    C;print @x;
    @x=@y.pop,@x[0..-$i-3],@v.pop,@x[-$i...];
    sleepy;
}
for 1..9 {C();say $U;for 1..500 {;};C();say $u;for 1..500 {;}}
say "Woot!";