Minimize
 DOLLARS: 10.89 pizza + 0.79 FRFries + 2.89 M.cherry + 2.59 MILK + 2.69 C.Milk
 + 0.47 P.towels + 1.49 !SUGARS! + 1.27 cereal + 3.29 SixPackX + 3.29 SixPackY
Subject To
 aluminum: 137 pizza + 71.4 SixPackX + 71.4 SixPackY >= 27230
 RED#2:    1.9 pizza + 18 M.cherry + 1.9 P.towels + 1.6 !SUGARS! + 1.6 cereal
        + 1.7 SixPackX + 1.7 SixPackY - RgRED#2 = 18390
 salt:       67.2 pizza + 36 FRFries + 2.6 M.cherry + 3.4 P.towels + 4.3 !SUGARS!
           + 4.3 cereal + 31.4 SixPackX + 31.4 SixPackY + 0.000067 Air >= 21740
 fat:      41.8 pizza + 210.6 FRFries + 243 MILK + 223 C.Milk + 1.3 SixPackX
           + 1.2 SixPackY - Rgfat = 30270
 Fiber:    3.4 pizza + 9.3 FRFries + 0.084 M.cherry + 0.45 P.towels
           + 1.78 !SUGARS! + 1.78 cereal - RgFiber = 4789
 CALCIUM:  45.2 MILK + 43.2 C.Milk - RgCALCIUM = 11460
 Sparkle:  - pizza - FRFries <= 10000
 Dirt:     pizza + FRFries >= -10000
Bounds
      FRFries >= 20
 500 <= M.cherry <= 800
 20 <= MILK <= 5000
 0 <= C.Milk <= 8888
 P.towels >= -27000
      !SUGARS! >= 144
 0 <= SixPackX <= 99999999
      SixPackY = 24
      Air Free
-10 <= RgRED#2 <= 0
 0 <= Rgfat <= 99999
-34789 <= RgFiber <= 0
 0 <= RgCALCIUM <= 240
End
