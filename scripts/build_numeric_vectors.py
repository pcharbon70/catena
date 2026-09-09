"""Independent exact-rational binary64 oracle. Uses Python Fraction and one float conversion."""
import json,struct,random
from fractions import Fraction
from pathlib import Path
rng=random.Random(105)
def f(bits):return struct.unpack('>d',bits.to_bytes(8,'big'))[0]
def bits(value):return int.from_bytes(struct.pack('>d',value),'big')
patterns=[0,1,2,3,0xfffffffffffff,0x10000000000000,0x7fefffffffffffff,0x3ff0000000000000,0x3ca0000000000000]
patterns += [p|(1<<63) for p in patterns]
patterns += [rng.randrange(0x7ff0000000000000)|(rng.randrange(2)<<63) for _ in range(400)]
vectors=[]
for ab in patterns:
 for bb in [patterns[rng.randrange(len(patterns))],0x3fe0000000000000,0x3ff0000000000000,0]:
  a,b=f(ab),f(bb)
  for op in ['add','subtract','multiply','divide']:
   if op=='divide' and b==0:expected='zero_divisor'
   else:
    x,y=Fraction(a),Fraction(b)
    value={'add':lambda:x+y,'subtract':lambda:x-y,'multiply':lambda:x*y,'divide':lambda:x/y}[op]()
    try:
     result=float(value)
     if result==0:
      # The exact rational loses zero's sign; prescribe the IEEE direction.
      if op in ['multiply','divide']:negative=(ab>>63)^(bb>>63)
      elif value!=0:negative=value<0
      elif op=='add':negative=(ab>>63)&(bb>>63)
      else:negative=(ab>>63)&(1^(bb>>63))
      result=-0.0 if negative else 0.0
     expected=bits(result)
    except OverflowError:expected='overflow'
   vectors.append([op,ab,bb,expected])
(Path(__file__).resolve().parent.parent / 'test/fixtures/numeric-rational-vectors.json').write_text(json.dumps(vectors,separators=(',',':'))+'\n')
print(len(vectors))
