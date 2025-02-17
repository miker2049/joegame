(defpackage joegame-data
  (:use :cl :jonathan)
  (:nicknames jdb)
  (:export *world-data*))

(in-package joegame-data)

(defvar *world-data* nil
  "A data structure containing mapobject..")

(defvar mapobjects nil
  "Objects made of images")

(defvar sounds nil
  "beep boops")

(defvar characters nil
  "People in the world")

(defvar images nil
  "files with pixel data")


(defvar platforms nil
  "chunks of moving land")


(setq mapobjects
      '(:|leafy-tree|
        (:|tile_config|
         (
          :|tiles| (

                    212 213 214 215
                    232 233 234 235
                    252 253 254 255
                    272 273 274 275
                    292 293 294 295
                    -1  313 314  -1

                    )
          :|collision| (
                        0 0 0 0
                        0 0 0 0
                        0 0 0 0
                        0 1 1 0
                        0 1 1 0
                        0 1 1 0
                        )
          :|texture| "browserquestextrude" :|width| 4)
         :|req_image| ("browserquestextrude"))
        :|grass-boulder|
        (:|tile_config|
         (:|tiles| (17 18 19
                    37 38 39
                    57 58 59
                    77 78 79)
          :|collision| (0 0 0
                        1 1 1
                        1 1 1
                        0 0 0)
          :|texture| "browserquestextrude" :|width| 3)
         :|req_image| ("browserquestextrude"))
        :|grass-floor-rock|
        (:|tile_config|
         (:|tiles| (256 257
                    276 277)
          :|collision| (1 1
                        1 1)
          :|texture| "browserquestextrude" :|width| 2)
         :|req_image| ("browserquestextrude"))

        :|grass-floor-rock-2|
        (:|tile_config|
         (:|tiles| (
                    258 259
                    278 279
                    )
          :|collision| (1 1
                        1 1)
          :|texture| "browserquestextrude" :|width| 2)
         :|req_image| ("browserquestextrude"))


        :|dead-tree|
        (:|tile_config|
         (:|tiles| (
                    793 794 795
                    813 814 815
                    833 834 835
                    853 854 855
                    873 874 875
                    893 894 895

                    )
          :|collision| (
                        0  0  0
                        0  0  0
                        0  0  0
                        0  1  0
                        0  1  0
                        0  0  0
                        )
          :|texture| "browserquestextrude" :|width| 3)
         :|req_image| ("browserquestextrude"))
        :|museum-vase|
        (:|tile_config|
         (:|tiles| (180 181 196 197 212 213) :|texture| "22_Museum" :|width| 2)
         :|req_image| ("22_Museum"))
        :|large-desert-tree|
        (:|tile_config|
         (:|tiles|
          (4598 4599 4600 4601 4602 4630 4631 4632 4633 4634 4662 4663 4664 4665 4666
           4694 4695 4696 4697 4698 4726 4727 4728 4729 4730 4758 4759 4760 4761
           4762)
          :|texture| "11_Camping_16x16_nograss" :|width| 5)
         :|req_image| ("11_Camping_16x16_nograss"))
        :|desert-tree-mini|
        (:|tile_config|
         (:|tiles| (4214 4215 4246 4247) :|texture| "11_Camping_16x16_nograss"
          :|width| 2)
         :|req_image| ("11_Camping_16x16_nograss"))
        :|desert-tree-apple-mini|
        (:|tile_config|
         (:|tiles| (4212 4213 4244 4245) :|texture| "11_Camping_16x16_nograss"
          :|width| 2)
         :|req_image| ("11_Camping_16x16_nograss"))
        :|desert-tree-mini-apple|
        (:|tile_config|
         (:|tiles| (4212 4213 4244 4245) :|texture| "11_Camping_16x16_nograss"
          :|width| 0)
         :|req_image| ("11_Camping_16x16_nograss"))
        :|desert-tree-poof|
        (:|tile_config|
         (:|tiles| (4209 4210 4211 4241 4242 4243 4273 4274 4275) :|texture|
          "11_Camping_16x16_nograss" :|width| 3)
         :|req_image| ("11_Camping_16x16_nograss"))
        :|desert-tree-bonzai|
        (:|tile_config|
         (:|tiles|
          (4204 4205 4206 4207 4208 4236 4237 4238 4239 4240 4268 4269 4270 4271
           4272)
          :|texture| "11_Camping_16x16_nograss" :|width| 5)
         :|req_image| ("11_Camping_16x16_nograss"))
        :|desert-tree-large|
        (:|tile_config|
         (:|tiles|
          (4200 4201 4202 4203 4232 4233 4234 4235 4264 4265 4266 4267 4296 4297 4298
           4299)
          :|texture| "11_Camping_16x16_nograss" :|width| 4)
         :|req_image| ("11_Camping_16x16_nograss"))
        :|infant-fruit-tree-fall|
        (:|tile_config|
         (:|tiles| (2680 2681 2712 2713) :|texture| "11_Camping_16x16_nograss"
          :|width| 2)
         :|req_image| ("11_Camping_16x16_nograss"))
        :|spade-tree-fall|
        (:|tile_config|
         (:|tiles| (2728 2729 2730 2731 2760 2761 2762 2763 2792 2793 2794 2795)
          :|texture| "11_Camping_16x16_nograss" :|width| 4)
         :|req_image| ("11_Camping_16x16_nograss"))
        :|cone-tree-fall|
        (:|tile_config|
         (:|tiles| (2804 2805 2836 2837 2868 2869 2900 2901) :|texture|
          "11_Camping_16x16_nograss" :|width| 2)
         :|req_image| ("11_Camping_16x16_nograss"))
        :|large-fall-tree|
        (:|tile_config|
         (:|tiles|
          (2934 2935 2936 2937 2938 2966 2967 2968 2969 2970 2998 2999 3000 3001 3002
           3030 3031 3032 3033 3034 3062 3063 3064 3065 3066 3094 3095 3096 3097
           3098)
          :|texture| "11_Camping_16x16_nograss" :|width| 5)
         :|req_image| ("11_Camping_16x16_nograss"))
        :|cone-fall-tall-bush|
        (:|tile_config|
         (:|tiles| (2996 2997 3028 3029 3060 3061 3092 3093) :|texture|
          "11_Camping_16x16_nograss" :|width| 2)
         :|req_image| ("11_Camping_16x16_nograss"))

        :|baby-tree-fall|
        (:|tile_config|
         (:|tiles| (3236 3237 3268 3269 3300 3301)
          :|collision| (1 1 1 1 1 1)
          :|texture| "11_Camping_16x16_nograss"
          :|width| 2)
         :|req_image| ("11_Camping_16x16_nograss"))

        :|large-bonsai|
        (:|tile_config|
         (:|tiles|
          (2540 2541 2542 2543 2544 2572 2573 2574 2575 2576 2604 2605 2606 2607
           2608)
          :|texture| "11_Camping_16x16_nograss" :|width| 5)
         :|req_image| ("11_Camping_16x16_nograss"))
        :|fall-elm|
        (:|tile_config|
         (:|tiles|
          (2536 2537 2538 2539 2568 2569 2570 2571 2600 2601 2602 2603 2632 2633 2634
           2635)
          :|texture| "11_Camping_16x16_nograss" :|width| 4)
         :|req_image| ("11_Camping_16x16_nograss"))
        :|fall-tree-big|
        (:|tile_config|
         (:|tiles|
          (2628 2629 2630 2631 2660 2661 2662 2663 2692 2693 2694 2695 2724 2725 2726
           2727)
          :|texture| "11_Camping_16x16_nograss" :|width| 4)
         :|req_image| ("11_Camping_16x16_nograss"))
        :|apple-tree-fall|
        (:|tile_config|
         (:|tiles|
          (2624 2625 2626 2627 2656 2657 2658 2659 2688 2689 2690 2691 2720 2721 2722
           2723)
          :|texture| "11_Camping_16x16_nograss" :|width| 4)
         :|req_image| ("11_Camping_16x16_nograss"))
        :|operating-table|
        (:|tile_config|
         (:|tiles| (1040 1041 1056 1057 1072 1073) :|texture| "19_Hospital_16x16"
          :|width| 2)
         :|req_image| ("19_Hospital_16x16"))
        :|seperator|
        (:|tile_config|
         (:|tiles| (381 382 383) :|texture| "19_Hospital_16x16" :|width| 3)
         :|req_image| ("19_Hospital_16x16"))
        :|potted-flowers|
        (:|tile_config|
         (:|tiles| (304 305 320 321 307 308 323 324) :|texture| "19_Hospital_16x16"
          :|pick| T)
         :|req_image| ("19_Hospital_16x16"))
        :|mri|
        (:|tile_config|
         (:|tiles| (597 598 613 614 629 630 645 646) :|texture| "19_Hospital_16x16"
          :|width| 2)
         :|req_image| ("19_Hospital_16x16"))
        :|waiting-room-seats|
        (:|tile_config|
         (:|tiles| (1002 1003 1004 1018 1019 1020) :|texture| "19_Hospital_16x16"
          :|width| 3)
         :|req_image| ("19_Hospital_16x16"))
        :|medical-tools|
        (:|tile_config|
         (:|tiles| (980 981 996 997) :|texture| "19_Hospital_16x16" :|width| 2)
         :|req_image| ("19_Hospital_16x16"))
        :|cubbies|
        (:|tile_config|
         (:|tiles| (1553 1554 1569 1570 1585 1586 1601 1602) :|texture|
          "19_Hospital_16x16" :|width| 2)
         :|req_image| ("19_Hospital_16x16"))
        :|play-creature-large-green|
        (:|tile_config|
         (:|tiles| (1402 1403 1418 1419) :|texture| "19_Hospital_16x16" :|width| 2)
         :|req_image| ("19_Hospital_16x16"))
        :|play-creature-large-yellow|
        (:|tile_config|
         (:|tiles| (1338 1339 1354 1355) :|texture| "19_Hospital_16x16" :|width| 2)
         :|req_image| ("19_Hospital_16x16"))
        :|play-creature-large-blue|
        (:|tile_config|
         (:|tiles| (1306 1307 1322 1323) :|texture| "19_Hospital_16x16" :|width| 2)
         :|req_image| ("19_Hospital_16x16"))
        :|play-creature-large|
        (:|tile_config|
         (:|tiles| (1274 1275 1290 1291) :|texture| "19_Hospital_16x16" :|width| 2)
         :|req_image| ("19_Hospital_16x16"))
        :|bio-workspace|
        (:|tile_config|
         (:|tiles| (1037 1038 1039 1053 1054 1055 1069 1070 1071) :|texture|
          "19_Hospital_16x16" :|width| 3)
         :|req_image| ("19_Hospital_16x16"))
        :|directory-thin|
        (:|tile_config|
         (:|tiles| (686 702 718) :|texture| "19_Hospital_16x16" :|width| 1)
         :|req_image| ("19_Hospital_16x16"))
        :|directory|
        (:|tile_config|
         (:|tiles| (684 685 700 701 716 717) :|texture| "19_Hospital_16x16" :|width|
          2)
         :|req_image| ("19_Hospital_16x16"))
        :|learning-skeleton|
        (:|tile_config|
         (:|tiles| (545 561 577) :|texture| "19_Hospital_16x16" :|width| 1)
         :|req_image| ("19_Hospital_16x16"))
        :|axe|
        (:|tile_config|
         (:|tiles| (49) :|texture| "tilesetformattedupdate1_redshrike" :|width| 1)
         :|req_image| ("tilesetformattedupdate1_redshrike"))
        :|lofi-bookshelf|
        (:|tile_config|
         (:|tiles| (45 46 52 53) :|texture| "tilesetformattedupdate1_redshrike"
          :|width| 2)
         :|req_image| ("tilesetformattedupdate1_redshrike"))
        :|fancy-rug2|
        (:|tile_config|
         (:|tiles| (3 4 5 10 11 12) :|texture| "tilesetformattedupdate1_redshrike"
          :|width| 3)
         :|req_image| ("tilesetformattedupdate1_redshrike"))
        :|fancy-rug|
        (:|tile_config|
         (:|tiles| (0 1 2 7 8 9) :|texture| "tilesetformattedupdate1_redshrike"
          :|width| 3)
         :|req_image| ("tilesetformattedupdate1_redshrike"))
        :|armor-set|
        (:|tile_config|
         (:|tiles| (31 38) :|texture| "tilesetformattedupdate1_redshrike" :|width| 1)
         :|req_image| ("tilesetformattedupdate1_redshrike"))
        :|tissue|
        (:|tile_config| (:|tiles| (35) :|texture| "3_Bathroom_16x16" :|width| 1)
         :|req_image| ("3_Bathroom_16x16"))
        :|sink-wood|
        (:|tile_config|
         (:|tiles| (128 129 144 145) :|texture| "3_Bathroom_16x16" :|width| 2)
         :|req_image| ("3_Bathroom_16x16"))
        :|washer|
        (:|tile_config|
         (:|tiles| (12 13 28 29 44 45) :|texture| "3_Bathroom_16x16" :|width| 2)
         :|req_image| ("3_Bathroom_16x16"))
        :|dirty-laundry|
        (:|tile_config| (:|tiles| (544 560) :|texture| "3_Bathroom_16x16" :|width| 1)
         :|req_image| ("3_Bathroom_16x16"))
        :|utility-sink|
        (:|tile_config|
         (:|tiles| (688 689 704 705 720 721) :|texture| "3_Bathroom_16x16" :|width|
          2)
         :|req_image| ("3_Bathroom_16x16"))
        :|broken-mirror|
        (:|tile_config| (:|tiles| (728 744) :|texture| "3_Bathroom_16x16" :|width| 1)
         :|req_image| ("3_Bathroom_16x16"))
        :|rubber-ducky|
        (:|tile_config| (:|tiles| (169) :|texture| "3_Bathroom_16x16" :|width| 1)
         :|req_image| ("3_Bathroom_16x16"))
        :|old-fashioned-bath|
        (:|tile_config|
         (:|tiles| (238 239 254 255 270 271 286 287) :|texture| "3_Bathroom_16x16"
          :|width| 2)
         :|req_image| ("3_Bathroom_16x16"))
        :|blackboard|
        (:|tile_config|
         (:|tiles| (90 91 106 107) :|texture| "5_Classroom_and_library_16x16"
          :|width| 2)
         :|req_image| ("5_Classroom_and_library_16x16"))
        :|computer|
        (:|tile_config|
         (:|tiles| (160 176) :|texture| "5_Classroom_and_library_16x16" :|width| 1)
         :|req_image| ("5_Classroom_and_library_16x16"))
        :|special-book|
        (:|tile_config|
         (:|tiles| (238 239 254 255 270 271) :|texture|
          "5_Classroom_and_library_16x16" :|width| 2)
         :|req_image| ("5_Classroom_and_library_16x16"))
        :|classroom-desk|
        (:|tile_config|
         (:|tiles| (2 3 18 19 34 35) :|texture| "5_Classroom_and_library_16x16"
          :|width| 2)
         :|req_image| ("5_Classroom_and_library_16x16"))
        :|classroom-desk-side|
        (:|tile_config|
         (:|tiles| (50 51 66 67) :|texture| "5_Classroom_and_library_16x16" :|width|
          2)
         :|req_image| ("5_Classroom_and_library_16x16"))
        :|librarian|
        (:|tile_config|
         (:|tiles| (171 172 173 174 175 187 188 189 190 191 203 204 205 206 207)
          :|texture| "5_Classroom_and_library_16x16" :|width| 5)
         :|req_image| ("5_Classroom_and_library_16x16"))
        :|lawn-chair|
        (:|tile_config| (:|tiles| (84 100) :|texture| "9_Fishing_16x16" :|width| 1)
         :|req_image| ("9_Fishing_16x16"))
        :|lawn-chair-fancy|
        (:|tile_config|
         (:|tiles| (70 71 72 86 87 88) :|texture| "9_Fishing_16x16" :|width| 3)
         :|req_image| ("9_Fishing_16x16"))
        :|foldable-stool|
        (:|tile_config| (:|tiles| (52 68) :|texture| "9_Fishing_16x16" :|width| 1)
         :|req_image| ("9_Fishing_16x16"))
        :|fishing-pole-rack|
        (:|tile_config|
         (:|tiles| (249 250 251 265 266 267 281 282 283) :|texture| "9_Fishing_16x16"
          :|width| 3)
         :|req_image| ("9_Fishing_16x16"))
        :|net-fishing|
        (:|tile_config|
         (:|tiles| (75 91 107) :|texture| "9_Fishing_16x16" :|width| 1) :|req_image|
         ("9_Fishing_16x16"))
        :|scissors|
        (:|tile_config| (:|tiles| (153) :|texture| "9_Fishing_16x16" :|width| 1)
         :|req_image| ("9_Fishing_16x16"))
        :|chair-with-umbrella|
        (:|tile_config|
         (:|tiles| (312 313 314 315 328 329 330 331 344 345 346 347) :|texture|
          "9_Fishing_16x16" :|width| 4)
         :|req_image| ("9_Fishing_16x16"))
        :|dead-fish|
        (:|tile_config|
         (:|tiles| (181 182 197 198) :|texture| "9_Fishing_16x16" :|width| 2)
         :|req_image| ("9_Fishing_16x16"))
        :|small-cooler|
        (:|tile_config| (:|tiles| (34 50) :|texture| "9_Fishing_16x16" :|width| 1)
         :|req_image| ("9_Fishing_16x16"))
        :|fishing-pole|
        (:|tile_config|
         (:|tiles| (23 24 39 40 55 56) :|texture| "9_Fishing_16x16" :|width| 2)
         :|req_image| ("9_Fishing_16x16"))
        :|cooler-yellow|
        (:|tile_config|
         (:|tiles| (98 99 114 115) :|texture| "9_Fishing_16x16" :|width| 2)
         :|req_image| ("9_Fishing_16x16"))
        :|cooler|
        (:|tile_config|
         (:|tiles| (130 131 146 147) :|texture| "9_Fishing_16x16" :|width| 2)
         :|req_image| ("9_Fishing_16x16"))
        :|pot|
        (:|tile_config| (:|tiles| (322) :|texture| "12_Kitchen_16x16" :|width| 1)
         :|req_image| ("12_Kitchen_16x16"))
        :|fancy-cake|
        (:|tile_config| (:|tiles| (764) :|texture| "12_Kitchen_16x16" :|width| 1)
         :|req_image| ("12_Kitchen_16x16"))
        :|strawberry-cake|
        (:|tile_config| (:|tiles| (744) :|texture| "12_Kitchen_16x16" :|width| 1)
         :|req_image| ("12_Kitchen_16x16"))
        :|donuts-plate|
        (:|tile_config| (:|tiles| (658) :|texture| "12_Kitchen_16x16" :|width| 1)
         :|req_image| ("12_Kitchen_16x16"))
        :|skillet|
        (:|tile_config| (:|tiles| (574 575) :|texture| "12_Kitchen_16x16" :|width| 2)
         :|req_image| ("12_Kitchen_16x16"))
        :|range|
        (:|tile_config| (:|tiles| (184 200) :|texture| "12_Kitchen_16x16" :|width| 1)
         :|req_image| ("12_Kitchen_16x16"))
        :|meal|
        (:|tile_config| (:|tiles| (352) :|texture| "12_Kitchen_16x16" :|width| 1)
         :|req_image| ("12_Kitchen_16x16"))
        :|espresso-machine-home|
        (:|tile_config| (:|tiles| (511 527) :|texture| "12_Kitchen_16x16" :|width| 1)
         :|req_image| ("12_Kitchen_16x16"))
        :|oranges|
        (:|tile_config| (:|tiles| (556) :|texture| "12_Kitchen_16x16" :|width| 1)
         :|req_image| ("12_Kitchen_16x16"))
        :|fryer|
        (:|tile_config|
         (:|tiles| (518 519 520 534 535 536) :|texture| "12_Kitchen_16x16" :|width|
          3)
         :|req_image| ("12_Kitchen_16x16"))
        :|bus-front-3|
        (:|tile_config|
         (:|tiles|
          (423 424 425 455 456 457 487 488 489 519 520 521 551 552 553 583 584 585
           615 616 617)
          :|texture| "10_Vehicles_16x16" :|width| 3)
         :|req_image| ("10_Vehicles_16x16"))
        :|green-sedan-side|
        (:|tile_config|
         (:|tiles| (288 289 290 291 320 321 322 323 352 353 354 355) :|texture|
          "10_Vehicles_16x16" :|width| 4)
         :|req_image| ("10_Vehicles_16x16"))
        :|silver-sedan-front|
        (:|tile_config|
         (:|tiles| (272 273 304 305 336 337 368 369) :|texture| "10_Vehicles_16x16"
          :|width| 2)
         :|req_image| ("10_Vehicles_16x16"))
        :|silver-sedan-side|
        (:|tile_config|
         (:|tiles| (300 301 302 303 332 333 334 335 364 365 366 367 396 397 398 399)
          :|texture| "10_Vehicles_16x16" :|width| 4)
         :|req_image| ("10_Vehicles_16x16"))
        :|red-sedan-side|
        (:|tile_config|
         (:|tiles| (44 45 46 47 76 77 78 79 108 109 110 111) :|texture|
          "10_Vehicles_16x16" :|width| 4)
         :|req_image| ("10_Vehicles_16x16"))
        :|bus-stop|
        (:|tile_config|
         (:|tiles| (826 827 828 829 830 858 859 860 861 862 890 891 892 893 894)
          :|texture| "10_Vehicles_16x16" :|width| 5)
         :|req_image| ("10_Vehicles_16x16"))
        :|rv|
        (:|tile_config|
         (:|tiles|
          (1780 1781 1782 1783 1784 1785 1812 1813 1814 1815 1816 1817 1844 1845 1846
           1847 1848 1849 1876 1877 1878 1879 1880 1881)
          :|texture| "10_Vehicles_16x16" :|width| 6)
         :|req_image| ("10_Vehicles_16x16"))
        :|hotdog-truck|
        (:|tile_config|
         (:|tiles|
          (1546 1547 1548 1549 1550 1551 1578 1579 1580 1581 1582 1583 1610 1611 1612
           1613 1614 1615 1642 1643 1644 1645 1646 1647 1674 1675 1676 1677 1678
           1679)
          :|texture| "10_Vehicles_16x16" :|width| 6)
         :|req_image| ("10_Vehicles_16x16"))
        :|peanut-stand|
        (:|tile_config|
         (:|tiles| (2375 2376 2377 2407 2408 2409 2439 2440 2441) :|texture|
          "10_Vehicles_16x16" :|width| 3)
         :|req_image| ("10_Vehicles_16x16"))
        :|gas-pump|
        (:|tile_config|
         (:|tiles|
          (2567 2568 2569 2570 2571 2572 2573 2599 2600 2601 2602 2603 2604 2605 2631
           2632 2633 2634 2635 2636 2637 2663 2664 2665 2666 2667 2668 2669 2695 2696
           2697 2698 2699 2700 2701 2727 2728 2729 2730 2731 2732 2733)
          :|texture| "10_Vehicles_16x16" :|width| 7)
         :|req_image| ("10_Vehicles_16x16"))
        :|helicopter-pad|
        (:|tile_config|
         (:|tiles|
          (2993 2994 2995 2996 2997 2998 2999 3000 3001 3002 3003 3004 3005 3006 3007
           3025 3026 3027 3028 3029 3030 3031 3032 3033 3034 3035 3036 3037 3038 3039
           3057 3058 3059 3060 3061 3062 3063 3064 3065 3066 3067 3068 3069 3070 3071
           3089 3090 3091 3092 3093 3094 3095 3096 3097 3098 3099 3100 3101 3102 3103
           3121 3122 3123 3124 3125 3126 3127 3128 3129 3130 3131 3132 3133 3134 3135
           3153 3154 3155 3156 3157 3158 3159 3160 3161 3162 3163 3164 3165 3166 3167
           3185 3186 3187 3188 3189 3190 3191 3192 3193 3194 3195 3196 3197 3198 3199
           3217 3218 3219 3220 3221 3222 3223 3224 3225 3226 3227 3228 3229 3230 3231
           3249 3250 3251 3252 3253 3254 3255 3256 3257 3258 3259 3260 3261 3262 3263
           3281 3282 3283 3284 3285 3286 3287 3288 3289 3290 3291 3292 3293 3294 3295
           3313 3314 3315 3316 3317 3318 3319 3320 3321 3322 3323 3324 3325 3326 3327
           3345 3346 3347 3348 3349 3350 3351 3352 3353 3354 3355 3356 3357 3358
           3359)
          :|texture| "10_Vehicles_16x16" :|width| 15)
         :|req_image| ("10_Vehicles_16x16"))
        :|helicopter|
        (:|tile_config|
         (:|tiles|
          (2741 2742 2743 2744 2745 2746 2747 2748 2749 2750 2773 2774 2775 2776 2777
           2778 2779 2780 2781 2782 2805 2806 2807 2808 2809 2810 2811 2812 2813 2814
           2837 2838 2839 2840 2841 2842 2843 2844 2845 2846 2869 2870 2871 2872 2873
           2874 2875 2876 2877 2878 2901 2902 2903 2904 2905 2906 2907 2908 2909 2910
           2933 2934 2935 2936 2937 2938 2939 2940 2941 2942 2965 2966 2967 2968 2969
           2970 2971 2972 2973 2974)
          :|texture| "10_Vehicles_16x16" :|width| 10)
         :|req_image| ("10_Vehicles_16x16"))
        :|flower-stand|
        (:|tile_config|
         (:|tiles|
          (2371 2372 2373 2403 2404 2405 2435 2436 2437 2467 2468 2469 2499 2500
           2501)
          :|texture| "10_Vehicles_16x16" :|width| 3)
         :|req_image| ("10_Vehicles_16x16"))
        :|ice-cream-truck|
        (:|tile_config|
         (:|tiles|
          (1832 1833 1834 1835 1836 1837 1838 1864 1865 1866 1867 1868 1869 1870 1896
           1897 1898 1899 1900 1901 1902 1928 1929 1930 1931 1932 1933 1934 1960 1961
           1962 1963 1964 1965 1966 1992 1993 1994 1995 1996 1997 1998)
          :|texture| "10_Vehicles_16x16" :|width| 7)
         :|req_image| ("10_Vehicles_16x16"))
        :|taco-truck|
        (:|tile_config|
         (:|tiles|
          (2112 2113 2114 2115 2116 2117 2118 2144 2145 2146 2147 2148 2149 2150 2176
           2177 2178 2179 2180 2181 2182 2208 2209 2210 2211 2212 2213 2214)
          :|texture| "10_Vehicles_16x16" :|width| 7)
         :|req_image| ("10_Vehicles_16x16"))
        :|yellow-bus|
        (:|tile_config|
         (:|tiles|
          (416 417 418 419 420 421 422 448 449 450 451 452 453 454 480 481 482 483
           484 485 486 512 513 514 515 516 517 518)
          :|texture| "10_Vehicles_16x16" :|width| 7)
         :|req_image| ("10_Vehicles_16x16"))
        :|turney-chair|
        (:|tile_config| (:|tiles| (179 195) :|texture| "1_Generic_16x16" :|width| 1)
         :|req_image| ("1_Generic_16x16"))
        :|wardrobe|
        (:|tile_config|
         (:|tiles| (1104 1105 1106 1120 1121 1122 1136 1137 1138) :|texture|
          "1_Generic_16x16" :|width| 3)
         :|req_image| ("1_Generic_16x16"))
        :|plunger|
        (:|tile_config|
         (:|tiles| (1133 1149) :|texture| "1_Generic_16x16" :|width| 1) :|req_image|
         ("1_Generic_16x16"))
        :|sink|
        (:|tile_config|
         (:|tiles| (1131 1132 1147 1148) :|texture| "1_Generic_16x16" :|width| 2)
         :|req_image| ("1_Generic_16x16"))
        :|c-table|
        (:|tile_config|
         (:|tiles| (870 871 886 887) :|texture| "1_Generic_16x16" :|width| 2)
         :|req_image| ("1_Generic_16x16"))
        :|telephone|
        (:|tile_config|
         (:|tiles| (634 635 650 651) :|texture| "1_Generic_16x16" :|width| 2)
         :|req_image| ("1_Generic_16x16"))
        :|elevator-doors|
        (:|tile_config|
         (:|tiles| (336 337 338 352 353 354) :|texture| "1_Generic_16x16" :|width| 3)
         :|req_image| ("1_Generic_16x16"))
        :|long-red-rug|
        (:|tile_config|
         (:|tiles| (359 360 361 362 375 376 377 378) :|texture| "1_Generic_16x16"
          :|width| 4)
         :|req_image| ("1_Generic_16x16"))
        :|big-purple-rug|
        (:|tile_config|
         (:|tiles| (348 349 350 351 364 365 366 367 380 381 382 383 396 397 398 399)
          :|texture| "1_Generic_16x16" :|width| 4)
         :|req_image| ("1_Generic_16x16"))
        :|stage-with-steps|
        (:|tile_config|
         (:|tiles| (16 17 18 19 20 32 33 34 35 36 64 65 66 67 68) :|texture|
          "13_Conference_Hall_16x16" :|width| 4)
         :|req_image| ("13_Conference_Hall_16x16"))
        :|fire-extinguisher|
        (:|tile_config|
         (:|tiles| (125 141) :|texture| "13_Conference_Hall_16x16" :|width| 1)
         :|req_image| ("13_Conference_Hall_16x16"))
        :|podium|
        (:|tile_config|
         (:|tiles| (40 56) :|texture| "13_Conference_Hall_16x16" :|width| 1)
         :|req_image| ("13_Conference_Hall_16x16"))
        :|chaise|
        (:|tile_config|
         (:|tiles| (684 685 700 701) :|texture| "14_Basement_16x16" :|width| 2)
         :|req_image| ("14_Basement_16x16"))
        :|pingpong-racket-ball|
        (:|tile_config|
         (:|tiles| (306 305) :|texture| "14_Basement_16x16" :|width| 2) :|req_image|
         ("14_Basement_16x16"))
        :|restraunt-boot-bench-side|
        (:|tile_config|
         (:|tiles| (77 78 93 94 109 110) :|texture| "14_Basement_16x16" :|width| 2)
         :|req_image| ("14_Basement_16x16"))
        :|bar-table|
        (:|tile_config|
         (:|tiles| (97 98 113 114) :|texture| "14_Basement_16x16" :|width| 2)
         :|req_image| ("14_Basement_16x16"))
        :|lcd-flatscreen|
        (:|tile_config|
         (:|tiles| (478 479 494 495) :|texture| "14_Basement_16x16" :|width| 2)
         :|req_image| ("14_Basement_16x16"))
        :|arcade-machine-side|
        (:|tile_config|
         (:|tiles| (658 674 690 675 691 659) :|texture| "14_Basement_16x16" :|width|
          2)
         :|req_image| ("14_Basement_16x16"))
        :|arcade-machine2|
        (:|tile_config|
         (:|tiles| (657 673 689) :|texture| "14_Basement_16x16" :|width| 1)
         :|req_image| ("14_Basement_16x16"))
        :|arcade-machine1|
        (:|tile_config|
         (:|tiles| (656 672 688) :|texture| "14_Basement_16x16" :|width| 1)
         :|req_image| ("14_Basement_16x16"))
        :|video-game-console3|
        (:|tile_config|
         (:|tiles| (752 753) :|texture| "14_Basement_16x16" :|width| 2) :|req_image|
         ("14_Basement_16x16"))
        :|video-game-console2|
        (:|tile_config|
         (:|tiles| (757 773) :|texture| "14_Basement_16x16" :|width| 1) :|req_image|
         ("14_Basement_16x16"))
        :|video-game-console1|
        (:|tile_config| (:|tiles| (756) :|texture| "14_Basement_16x16" :|width| 1)
         :|req_image| ("14_Basement_16x16"))
        :|pool-balls-racked|
        (:|tile_config| (:|tiles| (417) :|texture| "14_Basement_16x16" :|width| 1)
         :|req_image| ("14_Basement_16x16"))
        :|pool-cues|
        (:|tile_config|
         (:|tiles| (371 387 403) :|texture| "14_Basement_16x16" :|width| 1)
         :|req_image| ("14_Basement_16x16"))
        :|pool-table|
        (:|tile_config|
         (:|tiles| (404 405 406 407 420 421 422 423 436 437 438 439) :|texture|
          "14_Basement_16x16" :|width| 4)
         :|req_image| ("14_Basement_16x16"))
        :|pingpong-table|
        (:|tile_config|
         (:|tiles| (192 193 194 208 209 210 224 225 226 240 241 242) :|texture|
          "14_Basement_16x16" :|width| 3)
         :|req_image| ("14_Basement_16x16"))
        :|fancy-low-table|
        (:|tile_config|
         (:|tiles| (23 24 25 39 40 41 55 56 57) :|texture| "2_LivingRoom_16x16"
          :|width| 3)
         :|req_image| ("2_LivingRoom_16x16"))
        :|fancy-bench|
        (:|tile_config| (:|tiles| (21 22) :|texture| "2_LivingRoom_16x16" :|width| 2)
         :|req_image| ("2_LivingRoom_16x16"))
        :|fancystool|
        (:|tile_config| (:|tiles| (5) :|texture| "2_LivingRoom_16x16" :|width| 1)
         :|req_image| ("2_LivingRoom_16x16"))
        :|small-gold-lamp|
        (:|tile_config|
         (:|tiles| (222 238) :|texture| "2_LivingRoom_16x16" :|width| 1) :|req_image|
         ("2_LivingRoom_16x16"))
        :|clay-pot-silver-green|
        (:|tile_config|
         (:|tiles| (385 401) :|texture| "2_LivingRoom_16x16" :|width| 1) :|req_image|
         ("2_LivingRoom_16x16"))
        :|clay-pot2|
        (:|tile_config|
         (:|tiles| (384 400) :|texture| "2_LivingRoom_16x16" :|width| 1) :|req_image|
         ("2_LivingRoom_16x16"))
        :|clay-pot-plain|
        (:|tile_config|
         (:|tiles| (352 368) :|texture| "2_LivingRoom_16x16" :|width| 1) :|req_image|
         ("2_LivingRoom_16x16"))
        :|clay-pot-orange-bit|
        (:|tile_config|
         (:|tiles| (353 369) :|texture| "2_LivingRoom_16x16" :|width| 1) :|req_image|
         ("2_LivingRoom_16x16"))
        :|clay-pot|
        (:|tile_config|
         (:|tiles| (354 370) :|texture| "2_LivingRoom_16x16" :|width| 1) :|req_image|
         ("2_LivingRoom_16x16"))
        :|standing-mirror|
        (:|tile_config|
         (:|tiles| (360 361 376 377 392 393) :|texture| "2_LivingRoom_16x16" :|width|
          2)
         :|req_image| ("2_LivingRoom_16x16"))
        :|throw-pillow-yellow|
        (:|tile_config|
         (:|tiles| (494 495) :|texture| "2_LivingRoom_16x16" :|width| 2) :|req_image|
         ("2_LivingRoom_16x16"))
        :|throw-pillow|
        (:|tile_config|
         (:|tiles| (492 493) :|texture| "2_LivingRoom_16x16" :|width| 2) :|req_image|
         ("2_LivingRoom_16x16"))
        :|throw-pillows2|
        (:|tile_config|
         (:|tiles| (541 542 557 558) :|texture| "2_LivingRoom_16x16" :|width| 2)
         :|req_image| ("2_LivingRoom_16x16"))
        :|throw-pillows1|
        (:|tile_config|
         (:|tiles| (539 540 555 556) :|texture| "2_LivingRoom_16x16" :|width| 2)
         :|req_image| ("2_LivingRoom_16x16"))
        :|couchwhite|
        (:|tile_config|
         (:|tiles| (452 453 454 468 469 470) :|texture| "2_LivingRoom_16x16" :|width|
          3)
         :|req_image| ("2_LivingRoom_16x16"))
        :|couch|
        (:|tile_config|
         (:|tiles| (449 450 451 465 466 467) :|texture| "2_LivingRoom_16x16" :|width|
          3)
         :|req_image| ("2_LivingRoom_16x16"))
        :|globe|
        (:|tile_config|
         (:|tiles| (364 380) :|texture| "2_LivingRoom_16x16" :|width| 1) :|req_image|
         ("2_LivingRoom_16x16"))
        :|vanity|
        (:|tile_config|
         (:|tiles| (68 69 84 85 100 101) :|texture| "2_LivingRoom_16x16" :|width| 2)
         :|req_image| ("2_LivingRoom_16x16"))
        :|endpiecemirror|
        (:|tile_config|
         (:|tiles| (208 224) :|texture| "2_LivingRoom_16x16" :|width| 1) :|req_image|
         ("2_LivingRoom_16x16"))
        :|endpiece|
        (:|tile_config|
         (:|tiles| (209 225) :|texture| "2_LivingRoom_16x16" :|width| 1) :|req_image|
         ("2_LivingRoom_16x16"))
        :|broken-chair|
        (:|tile_config|
         (:|tiles| (199 215) :|texture| "2_LivingRoom_16x16" :|width| 1) :|req_image|
         ("2_LivingRoom_16x16"))
        :|chair-normal|
        (:|tile_config|
         (:|tiles| (300 316) :|texture| "2_LivingRoom_16x16" :|width| 1) :|req_image|
         ("2_LivingRoom_16x16"))
        :|house-plant-palm|
        (:|tile_config|
         (:|tiles| (13 14 29 30) :|texture| "2_LivingRoom_16x16" :|width| 2)
         :|req_image| ("2_LivingRoom_16x16"))
        :|house-plant-tree|
        (:|tile_config|
         (:|tiles| (10 11 26 27 42 43) :|texture| "2_LivingRoom_16x16" :|width| 2)
         :|req_image| ("2_LivingRoom_16x16"))
        :|coffeetable-with-books|
        (:|tile_config|
         (:|tiles| (116 132 118 134) :|texture| "2_LivingRoom_16x16" :|width| 2)
         :|req_image| ("2_LivingRoom_16x16"))
        :|coffee-table|
        (:|tile_config|
         (:|tiles| (116 117 132 133) :|texture| "2_LivingRoom_16x16" :|width| 2)
         :|req_image| ("2_LivingRoom_16x16"))
        :|bowl-fruit|
        (:|tile_config|
         (:|tiles| (146 162) :|texture| "2_LivingRoom_16x16" :|width| 1) :|req_image|
         ("2_LivingRoom_16x16"))
        :|bowl-tomatoes|
        (:|tile_config|
         (:|tiles| (145 161) :|texture| "2_LivingRoom_16x16" :|width| 1) :|req_image|
         ("2_LivingRoom_16x16"))
        :|bowl-squash|
        (:|tile_config|
         (:|tiles| (144 160) :|texture| "2_LivingRoom_16x16" :|width| 1) :|req_image|
         ("2_LivingRoom_16x16"))
        :|small-lamp|
        (:|tile_config|
         (:|tiles| (190 206) :|texture| "2_LivingRoom_16x16" :|width| 1) :|req_image|
         ("2_LivingRoom_16x16"))
        :|lamp|
        (:|tile_config|
         (:|tiles| (205 221 237) :|texture| "2_LivingRoom_16x16" :|width| 1)
         :|req_image| ("2_LivingRoom_16x16"))
        :|destroyed-cabinet|
        (:|tile_config|
         (:|tiles| (264 265 280 281) :|texture| "2_LivingRoom_16x16" :|width| 2)
         :|req_image| ("2_LivingRoom_16x16"))
        :|armydrum|
        (:|tile_config|
         (:|tiles| (214 215 230 231) :|texture| "6_Music_and_sport_16x16" :|width| 2)
         :|req_image| ("6_Music_and_sport_16x16"))
        :|subwoofer|
        (:|tile_config|
         (:|tiles| (7 23) :|texture| "6_Music_and_sport_16x16" :|width| 1)
         :|req_image| ("6_Music_and_sport_16x16"))
        :|pump|
        (:|tile_config|
         (:|tiles| (127 143) :|texture| "6_Music_and_sport_16x16" :|width| 1)
         :|req_image| ("6_Music_and_sport_16x16"))
        :|deflated-basketball|
        (:|tile_config|
         (:|tiles| (170) :|texture| "6_Music_and_sport_16x16" :|width| 1)
         :|req_image| ("6_Music_and_sport_16x16"))
        :|gold-trophy|
        (:|tile_config|
         (:|tiles| (408 424) :|texture| "6_Music_and_sport_16x16" :|width| 1)
         :|req_image| ("6_Music_and_sport_16x16"))
        :|silver-trophy|
        (:|tile_config|
         (:|tiles| (407 423) :|texture| "6_Music_and_sport_16x16" :|width| 1)
         :|req_image| ("6_Music_and_sport_16x16"))
        :|bronze-trophy|
        (:|tile_config|
         (:|tiles| (406 422) :|texture| "6_Music_and_sport_16x16" :|width| 1)
         :|req_image| ("6_Music_and_sport_16x16"))
        :|grandpiano|
        (:|tile_config|
         (:|tiles| (194 195 210 211 226 227) :|texture| "6_Music_and_sport_16x16"
          :|width| 2)
         :|req_image| ("6_Music_and_sport_16x16"))
        :|drumset|
        (:|tile_config|
         (:|tiles| (294 295 296 310 311 312) :|texture| "6_Music_and_sport_16x16"
          :|width| 3)
         :|req_image| ("6_Music_and_sport_16x16"))
        :|casiored|
        (:|tile_config|
         (:|tiles| (280 281) :|texture| "6_Music_and_sport_16x16" :|width| 2)
         :|req_image| ("6_Music_and_sport_16x16"))
        :|casio|
        (:|tile_config|
         (:|tiles| (282 283) :|texture| "6_Music_and_sport_16x16" :|width| 2)
         :|req_image| ("6_Music_and_sport_16x16"))
        :|bongo|
        (:|tile_config|
         (:|tiles| (190 206) :|texture| "6_Music_and_sport_16x16" :|width| 1)
         :|req_image| ("6_Music_and_sport_16x16"))
        :|drum|
        (:|tile_config|
         (:|tiles| (186 187 202 203) :|texture| "6_Music_and_sport_16x16" :|width| 2)
         :|req_image| ("6_Music_and_sport_16x16"))
        :|beachball|
        (:|tile_config|
         (:|tiles| (140 156) :|texture| "6_Music_and_sport_16x16" :|width| 1)
         :|req_image| ("6_Music_and_sport_16x16"))
        :|basketball|
        (:|tile_config|
         (:|tiles| (138 154) :|texture| "6_Music_and_sport_16x16" :|width| 1)
         :|req_image| ("6_Music_and_sport_16x16"))
        :|harp|
        (:|tile_config|
         (:|tiles| (13 14 29 30) :|texture| "6_Music_and_sport_16x16" :|width| 2)
         :|req_image| ("6_Music_and_sport_16x16"))
        :|electric-guitar|
        (:|tile_config|
         (:|tiles| (42 58 74) :|texture| "6_Music_and_sport_16x16" :|width| 1)
         :|req_image| ("6_Music_and_sport_16x16"))
        :|speaker|
        (:|tile_config|
         (:|tiles| (6 22 38) :|texture| "6_Music_and_sport_16x16" :|width| 1)
         :|req_image| ("6_Music_and_sport_16x16"))
        :|piano-bench|
        (:|tile_config|
         (:|tiles| (64 65 80 81) :|texture| "6_Music_and_sport_16x16" :|width| 2)
         :|req_image| ("6_Music_and_sport_16x16"))
        :|guitar|
        (:|tile_config|
         (:|tiles| (39 55 71) :|texture| "6_Music_and_sport_16x16" :|width| 1)
         :|req_image| ("6_Music_and_sport_16x16"))
        :|piano|
        (:|tile_config|
         (:|tiles| (16 17 32 33 48 49) :|texture| "6_Music_and_sport_16x16" :|width|
          2)
         :|req_image| ("6_Music_and_sport_16x16"))
        :|filled-cardboard-box2|
        (:|tile_config|
         (:|tiles| (1048 1049 1080 1081) :|texture| "6_Garage_Sales_16x16" :|width|
          2)
         :|req_image| ("6_Garage_Sales_16x16"))
        :|filled-small-cardboard-box|
        (:|tile_config|
         (:|tiles| (982 983 1014 1015) :|texture| "6_Garage_Sales_16x16" :|width| 2)
         :|req_image| ("6_Garage_Sales_16x16"))
        :|filled-cardboard-box|
        (:|tile_config|
         (:|tiles| (984 985 1016 1017) :|texture| "6_Garage_Sales_16x16" :|width| 2)
         :|req_image| ("6_Garage_Sales_16x16"))
        :|hvac-unit|
        (:|tile_config|
         (:|tiles| (785 786 817 818) :|texture| "6_Garage_Sales_16x16" :|width| 2)
         :|req_image| ("6_Garage_Sales_16x16"))
        :|open-cardboard-box|
        (:|tile_config|
         (:|tiles| (1041 1042 1073 1074) :|texture| "6_Garage_Sales_16x16" :|width|
          2)
         :|req_image| ("6_Garage_Sales_16x16"))
        :|carboard-box|
        (:|tile_config|
         (:|tiles| (977 978 1009 1010) :|texture| "6_Garage_Sales_16x16" :|width| 2)
         :|req_image| ("6_Garage_Sales_16x16"))
        :|spotlight|
        (:|tile_config|
         (:|tiles| (38 6 7 22 23 39) :|texture| "23_Tevelision_and_Film_Studio"
          :|width| 2)
         :|req_image| ("23_Tevelision_and_Film_Studio"))
        :|video-camera|
        (:|tile_config|
         (:|tiles| (32 33 48 49) :|texture| "23_Tevelision_and_Film_Studio" :|width|
          2)
         :|req_image| ("23_Tevelision_and_Film_Studio"))
        :|potted-plant|
        (:|tile_config|
         (:|tiles| (1082 1083 1098 1099 1114 1115) :|texture|
          "16_Grocery_store_16x16" :|pick| T)
         :|req_image| ("16_Grocery_store_16x16"))
        :|cheese-wheel2|
        (:|tile_config|
         (:|tiles| (922 938) :|texture| "16_Grocery_store_16x16" :|width| 1)
         :|req_image| ("16_Grocery_store_16x16"))
        :|cheese-wheel|
        (:|tile_config|
         (:|tiles| (921 937) :|texture| "16_Grocery_store_16x16" :|width| 1)
         :|req_image| ("16_Grocery_store_16x16"))
        :|cookie-rack|
        (:|tile_config|
         (:|tiles| (551 552 567 568) :|texture| "16_Grocery_store_16x16" :|width| 2)
         :|req_image| ("16_Grocery_store_16x16"))
        :|five-magazines|
        (:|tile_config|
         (:|tiles| (209 210 211 212) :|texture| "16_Grocery_store_16x16" :|pick| T)
         :|req_image| ("16_Grocery_store_16x16"))
        :|three-magazines|
        (:|tile_config|
         (:|tiles| (225 226 227 228) :|texture| "16_Grocery_store_16x16" :|pick| T)
         :|req_image| ("16_Grocery_store_16x16"))
        :|single-magazines|
        (:|tile_config|
         (:|tiles| (193 194 195 196) :|texture| "16_Grocery_store_16x16" :|pick| T)
         :|req_image| ("16_Grocery_store_16x16"))
        :|dead-leaves|
        (:|tile_config|
         (:|tiles| (117 118 119 137 138 139) :|texture| "browserquestextrude" :|pick|
          T)
         :|req_image| ("browserquestextrude"))
        :|rock|
        (:|body_config| (:|height| 15 :|width| 15 :|y| 1 :|x| 1)
         :|tile_config| (:|tiles| (97 98) :|collision| (1 1) :|texture| "browserquestextrude" :|width| 2)
         :|req_image| ("browserquestextrude"))


        :|palm-tree|
        (:|tile_config| (:|tiles| (1820 1821 1822 -1   -1
                                   1840 1841 1842 -1   -1
                                   1860 1861 1862 1863 1864
                                   1880 1881 1882 1883 1884
                                   1900 1901 1902 1903 1904)
                         :|collision| (0 0 0 0 0
                                       0 0 0 0 0
                                       0 0 0 0 0
                                       0 1 0 0 0
                                       0 1 0 0 0)
                         :|texture| "browserquestextrude"
                         :|width| 5)
         :|req_image| ("browserquestextrude"))


        :|tent|
        (:|tile_config| (:|tiles| (140 141 142
                                   160 161 162
                                   180 181 182
                                   200 201 202)
                         :|collision| (1 1 0
                                       1 1 0
                                       1 1 0
                                       0 0 0)
                         :|texture| "browserquestextrude"
                         :|width| 3)
         :|req_image| ("browserquestextrude"))

        :|well|
        (:|tile_config| (:|tiles| (280 281 282
                                   300 301 302
                                   320 321 322

                                   340 341 342
                                   360 361 362)
                         :|collision| (0 0 0
                                       0 0 0
                                       1 1 0
                                       1 1 0
                                       0 0 0)
                         :|texture| "browserquestextrude"
                         :|width| 3)
         :|req_image| ("browserquestextrude"))

        :|cow-skull|
        (:|tile_config|
         (:|tiles| (1726 1727 1728) :|texture| "browserquestextrude" :|width| 3)
         :|req_image| ("browserquestextrude"))
        :|shrub|
        (:|tile_config|
         (:|tiles| (311 312) :|collision| (0 0) :|texture| "browserquestextrude" :|width| 2)
         :|req_image| ("browserquestextrude"))
        :|cactus|
        (:|body_config| (:|height| 14 :|width| 10 :|y| 32 :|x| 8) :|tile_config|
         (:|tiles|
          (331 332
           351 352
           371 372)
          :|collision| (0 0
                        1 1
                        1 1)
          :|texture| "browserquestextrude"
          :|width| 2)
         :|req_image| ("browserquestextrude"))
        :|dead-tree3|
        (:|body_config| (:|height| 16 :|width| 16 :|y| 64 :|x| 16) :|tile_config|
         (:|tiles|
          (1677 1678 1679 1697 1698 1699 1717 1718 1719 1737 1738 1739 1757 1758
           1759)
          :|texture| "browserquestextrude" :|width| 3)
         :|req_image| ("browserquestextrude"))
        :|dead-tree2|
        (:|body_config| (:|height| 16 :|width| 16 :|y| 60 :|x| 25) :|tile_config|
         (:|tiles|
          (616 617 618 619 636 637 638 639 656 657 658 659 676 677 678 679 696 697
           698 699 716 717 718 719)
          :|texture| "browserquestextrude" :|width| 4)
         :|req_image| ("browserquestextrude"))
        :|dead-tree|
        (:|body_config| (:|height| 16 :|width| 16 :|y| 64 :|x| 16) :|tile_config|
         (:|tiles|
          (793 794 795 813 814 815 833 834 835 853 854 855 873 874 875 893 894 895)
          :|texture| "browserquestextrude" :|width| 3)
         :|req_image| ("browserquestextrude"))
        :|falling_leaves_emitter|
        (:|req_image| ("falling_leaves") :|name| "falling_leaves_emitter") :|sign1|
        (:|req_image| ("browserquestextrude") :|name| "sign1") :|constructionsp|
        (:|req_image| ("constructionman_sprite") :|name| "constructionsp")
        :|canyon-swirl| (:|req_image| ("null") :|name| "canyon-swirl") :|hormuz|
        (:|height| 300 :|width| 200 :|req_image| ("hormuz_oli_2020267_lrg")
                       :|texture| "hormuz_oli_2020267_lrg" :|name| "hormuz")
        :|shinyrock| (:|req_image| ("shinyrock1" "shinyrock2") :|name| "shinyrock")))


(setq sounds
      '(:|vowel|
        (:|splitLength| 1 :|url| "assets/audio/sounds/vowel.mp3" :|key| "vowel")
        :|item|
        (:|splitLength| 1 :|url| "assets/audio/sounds/item.mp3" :|key| "item")
        :|walk|
        (:|splitLength| 1 :|url| "assets/audio/sounds/walk.mp3" :|key| "walk") :|hit|
        (:|splitLength| 1 :|url| "assets/audio/sounds/hit.mp3" :|key| "hit")))

(setq images
      '(:|tf_zebra|
        (:|source| "finalbossblues -- http://www.timefantasy.net/" :|frameConfig|
         (:|columns| 3 :|tilecount| 12 :|imagewidth| 156 :|imageheight| 212
          :|spacing| 0 :|margin| 0 :|frameHeight| 53 :|frameWidth| 52)
         :|animLength| -1 :|key| "tf_zebra" :|url| "assets/images/zebra.png")
        :|tf_walrus|
        (:|source| "finalbossblues -- http://www.timefantasy.net/" :|frameConfig|
         (:|columns| 3 :|tilecount| 12 :|imagewidth| 126 :|imageheight| 144
          :|spacing| 0 :|margin| 0 :|frameHeight| 36 :|frameWidth| 42)
         :|animLength| -1 :|key| "tf_walrus" :|url| "assets/images/walrus.png")
        :|tf_vulture_sit|
        (:|source| "finalbossblues -- http://www.timefantasy.net/" :|frameConfig|
         (:|columns| 3 :|tilecount| 12 :|imagewidth| 126 :|imageheight| 144
          :|spacing| 0 :|margin| 0 :|frameHeight| 36 :|frameWidth| 42)
         :|animLength| -1 :|key| "tf_vulture_sit" :|url| "assets/images/vulture_sit.png")
        :|tf_vulture_fly|
        (:|source| "finalbossblues -- http://www.timefantasy.net/" :|frameConfig|
         (:|columns| 3 :|tilecount| 12 :|imagewidth| 126 :|imageheight| 144
          :|spacing| 0 :|margin| 0 :|frameHeight| 36 :|frameWidth| 42)
         :|animLength| -1 :|key| "tf_vulture_fly" :|url| "assets/images/vulture_fly.png")
        ;; :|tf_turtle|
        ;; (:|source| "finalbossblues -- http://www.timefantasy.net/" :|frameConfig|
        ;;  (:|columns| 3 :|tilecount| 12 :|imagewidth| 126 :|imageheight| 144
        ;;   :|spacing| 0 :|margin| 0 :|frameHeight| 36 :|frameWidth| 42)
        ;;  :|animLength| -1 :|key| "tf_turtle" )
        :|tf_toucan_sit|
        (:|source| "finalbossblues -- http://www.timefantasy.net/" :|frameConfig|
         (:|columns| 3 :|tilecount| 12 :|imagewidth| 126 :|imageheight| 144
          :|spacing| 0 :|margin| 0 :|frameHeight| 36 :|frameWidth| 42)
         :|animLength| -1 :|key| "tf_toucan_sit" :|url| "assets/images/toucan_sit.png")
        :|tf_toucan_fly|
        (:|source| "finalbossblues -- http://www.timefantasy.net/" :|frameConfig|
         (:|columns| 3 :|tilecount| 12 :|imagewidth| 126 :|imageheight| 144
          :|spacing| 0 :|margin| 0 :|frameHeight| 36 :|frameWidth| 42)
         :|animLength| -1 :|key| "tf_toucan_fly" :|url| "assets/images/toucan_fly.png")
        :|tf_tiger_cub|
        (:|source| "finalbossblues -- http://www.timefantasy.net/" :|frameConfig|
         (:|columns| 3 :|tilecount| 12 :|imagewidth| 126 :|imageheight| 144
          :|spacing| 0 :|margin| 0 :|frameHeight| 36 :|frameWidth| 42)
         :|animLength| -1 :|key| "tf_tiger_cub" :|url| "assets/images/tiger_cub.png")
        :|tf_tiger|
        (:|source| "finalbossblues -- http://www.timefantasy.net/" :|frameConfig|
         (:|columns| 3 :|tilecount| 12 :|imagewidth| 126 :|imageheight| 144
          :|spacing| 0 :|margin| 0 :|frameHeight| 36 :|frameWidth| 42)
         :|animLength| -1 :|key| "tf_tiger" :|url| "assets/images/tiger.png")
        ;; :|tf_swordfish|
        ;; (:|source| "finalbossblues -- http://www.timefantasy.net/" :|frameConfig|
        ;;  (:|columns| 3 :|tilecount| 12 :|imagewidth| 126 :|imageheight| 144
        ;;   :|spacing| 0 :|margin| 0 :|frameHeight| 36 :|frameWidth| 42)
        ;;  :|animLength| -1 :|key| "tf_swordfish" :|url| "assets/images/swordfish.png")
        ;; :|tf_shark_full_brown_1|
        ;; (:|source| "finalbossblues -- http://www.timefantasy.net/" :|frameConfig|
        ;;  (:|columns| 3 :|tilecount| 12 :|imagewidth| 192 :|imageheight| 192
        ;;   :|spacing| 0 :|margin| 0 :|frameHeight| 48 :|frameWidth| 64)
        ;;  :|animLength| -1 :|key| "tf_shark_full_brown_1")
        ;; :|tf_shark_full_blue_1|
        ;; (:|source| "finalbossblues -- http://www.timefantasy.net/" :|frameConfig|
        ;;  (:|columns| 3 :|tilecount| 12 :|imagewidth| 192 :|imageheight| 192
        ;;   :|spacing| 0 :|margin| 0 :|frameHeight| 48 :|frameWidth| 64)
        ;;  :|animLength| -1 :|key| "tf_shark_full_blue_1")
        ;; :|tf_shark_fin_brown_1|
        ;; (:|source| "finalbossblues -- http://www.timefantasy.net/" :|frameConfig|
        ;;  (:|columns| 3 :|tilecount| 12 :|imagewidth| 192 :|imageheight| 192
        ;;   :|spacing| 0 :|margin| 0 :|frameHeight| 48 :|frameWidth| 64)
        ;;  :|animLength| -1 :|key| "tf_shark_fin_brown_1")
        ;; :|tf_shark_fin_blue_1|
        ;; (:|source| "finalbossblues -- http://www.timefantasy.net/" :|frameConfig|
        ;;  (:|columns| 3 :|tilecount| 12 :|imagewidth| 192 :|imageheight| 192
        ;;   :|spacing| 0 :|margin| 0 :|frameHeight| 48 :|frameWidth| 64)
        ;;  :|animLength| -1 :|key| "tf_shark_fin_blue_1")
        ;; :|tf_shark_finshadow_brown_1|
        ;; (:|source| "finalbossblues -- http://www.timefantasy.net/" :|frameConfig|
        ;;  (:|columns| 3 :|tilecount| 12 :|imagewidth| 192 :|imageheight| 192
        ;;   :|spacing| 0 :|margin| 0 :|frameHeight| 48 :|frameWidth| 64)
        ;;  :|animLength| -1 :|key| "tf_shark_finshadow_brown_1")
        ;; :|tf_shark_finshadow_blue_1|
        ;; (:|source| "finalbossblues -- http://www.timefantasy.net/" :|frameConfig|
        ;;  (:|columns| 3 :|tilecount| 12 :|imagewidth| 192 :|imageheight| 192
        ;;   :|spacing| 0 :|margin| 0 :|frameHeight| 48 :|frameWidth| 64)
        ;;  :|animLength| -1 :|key| "tf_shark_finshadow_blue_1")
        :|tf_seaturtle|
        (:|source| "finalbossblues -- http://www.timefantasy.net/" :|frameConfig|
         (:|columns| 3 :|tilecount| 12 :|imagewidth| 126 :|imageheight| 144
          :|spacing| 0 :|margin| 0 :|frameHeight| 36 :|frameWidth| 42)
         :|animLength| -1 :|key| "tf_seaturtle" :|url| "assets/images/seaturtle.png")
        :|tf_seal|
        (:|source| "finalbossblues -- http://www.timefantasy.net/" :|frameConfig|
         (:|columns| 3 :|tilecount| 12 :|imagewidth| 126 :|imageheight| 144
          :|spacing| 0 :|margin| 0 :|frameHeight| 36 :|frameWidth| 42)
         :|animLength| -1 :|key| "tf_seal" :|url| "assets/images/seal.png")
        :|tf_sabretooth|
        (:|source| "finalbossblues -- http://www.timefantasy.net/" :|frameConfig|
         (:|columns| 3 :|tilecount| 12 :|imagewidth| 126 :|imageheight| 144
          :|spacing| 0 :|margin| 0 :|frameHeight| 36 :|frameWidth| 42)
         :|animLength| -1 :|key| "tf_sabretooth" :|url| "assets/images/sabretooth.png")
        :|tf_rhinoceros|
        (:|source| "finalbossblues -- http://www.timefantasy.net/" :|frameConfig|
         (:|columns| 3 :|tilecount| 12 :|imagewidth| 156 :|imageheight| 212
          :|spacing| 0 :|margin| 0 :|frameHeight| 53 :|frameWidth| 52)
         :|animLength| -1 :|key| "tf_rhinoceros" :|url| "assets/images/rhinoceros.png")
        ;; :|tf_ray|
        ;; (:|source| "finalbossblues -- http://www.timefantasy.net/" :|frameConfig|
        ;;  (:|columns| 3 :|tilecount| 12 :|imagewidth| 126 :|imageheight| 144
        ;;   :|spacing| 0 :|margin| 0 :|frameHeight| 36 :|frameWidth| 42)
        ;;  :|animLength| -1 :|key| "tf_ray" :|url| "assets/images/seaturtle.png")
        ;; :|tf_pufferfish_small|
        ;; (:|source| "finalbossblues -- http://www.timefantasy.net/" :|frameConfig|
        ;;  (:|columns| 3 :|tilecount| 12 :|imagewidth| 126 :|imageheight| 144
        ;;   :|spacing| 0 :|margin| 0 :|frameHeight| 36 :|frameWidth| 42)
        ;;  :|animLength| -1 :|key| "tf_pufferfish_small")
        ;; :|tf_pufferfish_big|
        ;; (:|source| "finalbossblues -- http://www.timefantasy.net/" :|frameConfig|
        ;;  (:|columns| 3 :|tilecount| 12 :|imagewidth| 126 :|imageheight| 144
        ;;   :|spacing| 0 :|margin| 0 :|frameHeight| 36 :|frameWidth| 42)
        ;;  :|animLength| -1 :|key| "tf_pufferfish_big")
        :|tf_penguin_baby|
        (:|source| "finalbossblues -- http://www.timefantasy.net/" :|frameConfig|
         (:|columns| 3 :|tilecount| 12 :|imagewidth| 126 :|imageheight| 144
          :|spacing| 0 :|margin| 0 :|frameHeight| 36 :|frameWidth| 42)
         :|animLength| -1 :|key| "tf_penguin_baby" :|url| "assets/images/penguin_baby.png")
        :|tf_penguin|
        (:|source| "finalbossblues -- http://www.timefantasy.net/" :|frameConfig|
         (:|columns| 3 :|tilecount| 12 :|imagewidth| 126 :|imageheight| 144
          :|spacing| 0 :|margin| 0 :|frameHeight| 36 :|frameWidth| 42)
         :|animLength| -1 :|key| "tf_penguin" :|url| "assets/images/penguin.png")
        :|tf_parrot_sit|
        (:|source| "finalbossblues -- http://www.timefantasy.net/" :|frameConfig|
         (:|columns| 3 :|tilecount| 12 :|imagewidth| 126 :|imageheight| 144
          :|spacing| 0 :|margin| 0 :|frameHeight| 36 :|frameWidth| 42)
         :|animLength| -1 :|key| "tf_parrot_sit" :|url| "assets/images/parrot_sit.png")
        :|tf_parrot_fly|
        (:|source| "finalbossblues -- http://www.timefantasy.net/" :|frameConfig|
         (:|columns| 3 :|tilecount| 12 :|imagewidth| 126 :|imageheight| 144
          :|spacing| 0 :|margin| 0 :|frameHeight| 36 :|frameWidth| 42)
         :|animLength| -1 :|key| "tf_parrot_fly" :|url| "assets/images/parrot_fly.png")
        :|tf_panther_cub|
        (:|source| "finalbossblues -- http://www.timefantasy.net/" :|frameConfig|
         (:|columns| 3 :|tilecount| 12 :|imagewidth| 126 :|imageheight| 144
          :|spacing| 0 :|margin| 0 :|frameHeight| 36 :|frameWidth| 42)
         :|animLength| -1 :|key| "tf_panther_cub" :|url| "assets/images/panther_cub.png")
        :|tf_panther|
        (:|source| "finalbossblues -- http://www.timefantasy.net/" :|frameConfig|
         (:|columns| 3 :|tilecount| 12 :|imagewidth| 126 :|imageheight| 144
          :|spacing| 0 :|margin| 0 :|frameHeight| 36 :|frameWidth| 42)
         :|animLength| -1 :|key| "tf_panther" :|url| "assets/images/panther.png")
        :|tf_owl_sit|
        (:|source| "finalbossblues -- http://www.timefantasy.net/" :|frameConfig|
         (:|columns| 3 :|tilecount| 12 :|imagewidth| 126 :|imageheight| 144
          :|spacing| 0 :|margin| 0 :|frameHeight| 36 :|frameWidth| 42)
         :|animLength| -1 :|key| "tf_owl_sit" :|url| "assets/images/owl_sit.png")
        :|tf_owl_fly|
        (:|source| "finalbossblues -- http://www.timefantasy.net/" :|frameConfig|
         (:|columns| 3 :|tilecount| 12 :|imagewidth| 126 :|imageheight| 144
          :|spacing| 0 :|margin| 0 :|frameHeight| 36 :|frameWidth| 42)
         :|animLength| -1 :|key| "tf_owl_fly" :|url| "assets/images/owl_fly.png")
        ;; :|tf_monkey_8sheet|
        ;; (:|source| "finalbossblues -- http://www.timefantasy.net/" :|frameConfig|
        ;;  (:|columns| 3 :|tilecount| 12 :|imagewidth| 312 :|imageheight| 288
        ;;   :|spacing| 0 :|margin| 0 :|frameHeight| 72 :|frameWidth| 104)
        ;;  :|animLength| -1 :|key| "tf_monkey_8sheet")
        :|tf_mammoth|
        (:|source| "finalbossblues -- http://www.timefantasy.net/" :|frameConfig|
         (:|columns| 3 :|tilecount| 12 :|imagewidth| 156 :|imageheight| 212
          :|spacing| 0 :|margin| 0 :|frameHeight| 53 :|frameWidth| 52)
         :|animLength| -1 :|key| "tf_mammoth" :|url| "assets/images/mammoth.png")
        :|tf_lizard|
        (:|source| "finalbossblues -- http://www.timefantasy.net/" :|frameConfig|
         (:|columns| 3 :|tilecount| 12 :|imagewidth| 60 :|imageheight| 80 :|spacing|
          0 :|margin| 0 :|frameHeight| 20 :|frameWidth| 20)
         :|animLength| -1 :|key| "tf_lizard" :|url| "assets/images/lizard.png")
        :|tf_lion_cub|
        (:|source| "finalbossblues -- http://www.timefantasy.net/" :|frameConfig|
         (:|columns| 3 :|tilecount| 12 :|imagewidth| 126 :|imageheight| 144
          :|spacing| 0 :|margin| 0 :|frameHeight| 36 :|frameWidth| 42)
         :|animLength| -1 :|key| "tf_lion_cub" :|url| "assets/images/lion_cub.png")
        :|tf_lioness|
        (:|source| "finalbossblues -- http://www.timefantasy.net/" :|frameConfig|
         (:|columns| 3 :|tilecount| 12 :|imagewidth| 126 :|imageheight| 144
          :|spacing| 0 :|margin| 0 :|frameHeight| 36 :|frameWidth| 42)
         :|animLength| -1 :|key| "tf_lioness" :|url| "assets/images/lioness.png")
        :|tf_lion|
        (:|source| "finalbossblues -- http://www.timefantasy.net/" :|frameConfig|
         (:|columns| 3 :|tilecount| 12 :|imagewidth| 126 :|imageheight| 144
          :|spacing| 0 :|margin| 0 :|frameHeight| 36 :|frameWidth| 42)
         :|animLength| -1 :|key| "tf_lion" :|url| "assets/images/lion.png")
        :|tf_kangaroo|
        (:|source| "finalbossblues -- http://www.timefantasy.net/" :|frameConfig|
         (:|columns| 3 :|tilecount| 12 :|imagewidth| 156 :|imageheight| 212
          :|spacing| 0 :|margin| 0 :|frameHeight| 53 :|frameWidth| 52)
         :|animLength| -1 :|key| "tf_kangaroo" :|url| "assets/images/kangaroo.png")
        ;; :|tf_jungle_tileset|
        ;; (:|source| "finalbossblues -- http://www.timefantasy.net/" :|frameConfig|
        ;;  (:|columns| 4 :|tilecount| 16 :|imagewidth| 352 :|imageheight| 336
        ;;   :|spacing| 0 :|margin| 0 :|frameHeight| 84 :|frameWidth| 117)
        ;;  :|animLength| -1 :|key| "tf_jungle_tileset")
        :|tf_horseshoe_crab|
        (:|source| "finalbossblues -- http://www.timefantasy.net/" :|frameConfig|
         (:|columns| 3 :|tilecount| 12 :|imagewidth| 126 :|imageheight| 144
          :|spacing| 0 :|margin| 0 :|frameHeight| 36 :|frameWidth| 42)
         :|animLength| -1 :|key| "tf_horseshoe_crab" :|url| "assets/images/horseshoe_crab.png")
        ;; :|tf_hippo_water|
        ;; (:|source| "finalbossblues -- http://www.timefantasy.net/" :|frameConfig|
        ;;  (:|columns| 3 :|tilecount| 12 :|imagewidth| 156 :|imageheight| 212
        ;;   :|spacing| 0 :|margin| 0 :|frameHeight| 53 :|frameWidth| 52)
        ;;  :|animLength| -1 :|key| "tf_hippo_water" )
        :|tf_hippo|
        (:|source| "finalbossblues -- http://www.timefantasy.net/" :|frameConfig|
         (:|columns| 3 :|tilecount| 12 :|imagewidth| 156 :|imageheight| 212
          :|spacing| 0 :|margin| 0 :|frameHeight| 53 :|frameWidth| 52)
         :|animLength| -1 :|key| "tf_hippo" :|url| "assets/images/hippo.png")
        :|tf_gorilla|
        (:|source| "finalbossblues -- http://www.timefantasy.net/" :|frameConfig|
         (:|columns| 3 :|tilecount| 12 :|imagewidth| 126 :|imageheight| 144
          :|spacing| 0 :|margin| 0 :|frameHeight| 36 :|frameWidth| 42)
         :|animLength| -1 :|key| "tf_gorilla" :|url| "assets/images/gorilla.png")
        :|tf_frog|
        (:|source| "finalbossblues -- http://www.timefantasy.net/" :|frameConfig|
         (:|columns| 3 :|tilecount| 12 :|imagewidth| 60 :|imageheight| 80 :|spacing|
          0 :|margin| 0 :|frameHeight| 20 :|frameWidth| 20)
         :|animLength| -1 :|key| "tf_frog" :|url| "assets/images/frog.png")
        ;; :|tf_fish_8sheet|
        ;; (:|source| "finalbossblues -- http://www.timefantasy.net/" :|frameConfig|
        ;;  (:|columns| 3 :|tilecount| 12 :|imagewidth| 192 :|imageheight| 128
        ;;   :|spacing| 0 :|margin| 0 :|frameHeight| 32 :|frameWidth| 64)
        ;;  :|animLength| -1 :|key| "tf_fish_8sheet")
        :|tf_elephant_baby|
        (:|source| "finalbossblues -- http://www.timefantasy.net/" :|frameConfig|
         (:|columns| 3 :|tilecount| 12 :|imagewidth| 156 :|imageheight| 212
          :|spacing| 0 :|margin| 0 :|frameHeight| 53 :|frameWidth| 52)
         :|animLength| -1 :|key| "tf_elephant_baby" :|url| "assets/images/elephant_baby.png")
        :|tf_elephant|
        (:|source| "finalbossblues -- http://www.timefantasy.net/" :|frameConfig|
         (:|columns| 3 :|tilecount| 12 :|imagewidth| 156 :|imageheight| 212
          :|spacing| 0 :|margin| 0 :|frameHeight| 53 :|frameWidth| 52)
         :|animLength| -1 :|key| "tf_elephant" :|url| "assets/images/elephant.png")
        ;; :|tf_crocodile_water|
        ;; (:|source| "finalbossblues -- http://www.timefantasy.net/" :|frameConfig|
        ;;  (:|columns| 3 :|tilecount| 12 :|imagewidth| 228 :|imageheight| 212
        ;;   :|spacing| 0 :|margin| 0 :|frameHeight| 53 :|frameWidth| 76)
        ;;  :|animLength| -1 :|key| "tf_crocodile_water")
        ;; :|tf_crocodile|
        ;; (:|source| "finalbossblues -- http://www.timefantasy.net/" :|frameConfig|
        ;;  (:|columns| 3 :|tilecount| 12 :|imagewidth| 228 :|imageheight| 212
        ;;   :|spacing| 0 :|margin| 0 :|frameHeight| 53 :|frameWidth| 76)
        ;;  :|animLength| -1 :|key| "tf_crocodile")
        ;; :|tf_crab|
        ;; (:|source| "finalbossblues -- http://www.timefantasy.net/" :|frameConfig|
        ;;  (:|columns| 3 :|tilecount| 12 :|imagewidth| 126 :|imageheight| 144
        ;;   :|spacing| 0 :|margin| 0 :|frameHeight| 36 :|frameWidth| 42)
        ;;  :|animLength| -1 :|key| "tf_crab")
        :|tf_camel_b_pack|
        (:|source| "finalbossblues -- http://www.timefantasy.net/" :|frameConfig|
         (:|columns| 3 :|tilecount| 12 :|imagewidth| 156 :|imageheight| 212
          :|spacing| 0 :|margin| 0 :|frameHeight| 53 :|frameWidth| 52)
         :|animLength| -1 :|key| "tf_camel_b_pack" :|url| "assets/images/camel_b_pack.png")
        :|tf_camel_b|
        (:|source| "finalbossblues -- http://www.timefantasy.net/" :|frameConfig|
         (:|columns| 3 :|tilecount| 12 :|imagewidth| 156 :|imageheight| 212
          :|spacing| 0 :|margin| 0 :|frameHeight| 53 :|frameWidth| 52)
         :|animLength| -1 :|key| "tf_camel_b" :|url| "assets/images/camel_b.png")
        :|tf_camel_a_pack|
        (:|source| "finalbossblues -- http://www.timefantasy.net/" :|frameConfig|
         (:|columns| 3 :|tilecount| 12 :|imagewidth| 156 :|imageheight| 212
          :|spacing| 0 :|margin| 0 :|frameHeight| 53 :|frameWidth| 52)
         :|animLength| -1 :|key| "tf_camel_a_pack" :|url| "assets/images/camel_a_pack.png")
        :|tf_camel_a|
        (:|source| "finalbossblues -- http://www.timefantasy.net/" :|frameConfig|
         (:|columns| 3 :|tilecount| 12 :|imagewidth| 156 :|imageheight| 212
          :|spacing| 0 :|margin| 0 :|frameHeight| 53 :|frameWidth| 52)
         :|animLength| -1 :|key| "tf_camel_a" :|url| "assets/images/camel_a.png")
        :|tf_bugs_8sheet|
        (:|source| "finalbossblues -- http://www.timefantasy.net/" :|frameConfig|
         (:|columns| 3 :|tilecount| 12 :|imagewidth| 192 :|imageheight| 128
          :|spacing| 0 :|margin| 0 :|frameHeight| 32 :|frameWidth| 64)
         :|animLength| -1 :|key| "tf_bugs_8sheet" :|url| "assets/images/bugs_8sheet.png")
        :|tf_buffalo|
        (:|source| "finalbossblues -- http://www.timefantasy.net/" :|frameConfig|
         (:|columns| 3 :|tilecount| 12 :|imagewidth| 156 :|imageheight| 212
          :|spacing| 0 :|margin| 0 :|frameHeight| 53 :|frameWidth| 52)
         :|animLength| -1 :|key| "tf_buffalo" :|url| "assets/images/buffalo.png")
        :|tf_bison|
        (:|source| "finalbossblues -- http://www.timefantasy.net/" :|frameConfig|
         (:|columns| 3 :|tilecount| 12 :|imagewidth| 156 :|imageheight| 212
          :|spacing| 0 :|margin| 0 :|frameHeight| 53 :|frameWidth| 52)
         :|animLength| -1 :|key| "tf_bison" :|url| "assets/images/buffalo.png")
        :|tf_birds_8sheet|
        (:|source| "finalbossblues -- http://www.timefantasy.net/" :|frameConfig|
         (:|columns| 3 :|tilecount| 12 :|imagewidth| 504 :|imageheight| 288
          :|spacing| 0 :|margin| 0 :|frameHeight| 72 :|frameWidth| 168)
         :|animLength| -1 :|key| "tf_birds_8sheet" :|url| "assets/images/birds_8sheet.png")
        :|tf_bee|
        (:|source| "finalbossblues -- http://www.timefantasy.net/" :|frameConfig|
         (:|columns| 3 :|tilecount| 12 :|imagewidth| 78 :|imageheight| 144 :|spacing|
          0 :|margin| 0 :|frameHeight| 36 :|frameWidth| 26)
         :|animLength| -1 :|key| "tf_bee" :|url| "assets/images/bee.png")
        :|13_School_16x16|
        (:|source| "limezu -- https://limezu.itch.io/" :|frameConfig|
         (:|columns| 32 :|tilecount| 3168 :|imagewidth| 512 :|imageheight| 1584
          :|spacing| 0 :|margin| 0 :|frameHeight| 16 :|frameWidth| 16)
         :|animLength| -1 :|key| "13_School_16x16")
        :|12_Hotel_and_Hospital_16x16|
        (:|source| "limezu -- https://limezu.itch.io/" :|frameConfig|
         (:|columns| 32 :|tilecount| 1984 :|imagewidth| 512 :|imageheight| 992
          :|spacing| 0 :|margin| 0 :|frameHeight| 16 :|frameWidth| 16)
         :|animLength| -1 :|key| "12_Hotel_and_Hospital_16x16")
        :|11_Camping_16x16_nograss|
        (:|source| "limezu -- https://limezu.itch.io/" :|frameConfig|
         (:|columns| 32 :|tilecount| 5408 :|imagewidth| 512 :|imageheight| 2704
          :|spacing| 0 :|margin| 0 :|frameHeight| 16 :|frameWidth| 16)
         :|animLength| -1 :|key| "11_Camping_16x16_nograss"
         :|url| "assets/images/11_Camping_16x16_nograss.png")
        :|11_Camping_16x16|
        (:|source| "limezu -- https://limezu.itch.io/" :|frameConfig|
         (:|columns| 32 :|tilecount| 5408 :|imagewidth| 512 :|imageheight| 2704
          :|spacing| 0 :|margin| 0 :|frameHeight| 16 :|frameWidth| 16)
         :|animLength| -1 :|key| "11_Camping_16x16")
        :|10_Vehicles_16x16|
        (:|source| "limezu -- https://limezu.itch.io/" :|frameConfig|
         (:|columns| 32 :|tilecount| 3456 :|imagewidth| 512 :|imageheight| 1728
          :|spacing| 0 :|margin| 0 :|frameHeight| 16 :|frameWidth| 16)
         :|animLength| -1 :|key| "10_Vehicles_16x16")
        :|9_Shopping_Center_and_Markets_16x16|
        (:|source| "limezu -- https://limezu.itch.io/" :|frameConfig|
         (:|columns| 32 :|tilecount| 2176 :|imagewidth| 512 :|imageheight| 1088
          :|spacing| 0 :|margin| 0 :|frameHeight| 16 :|frameWidth| 16)
         :|animLength| -1 :|key| "9_Shopping_Center_and_Markets_16x16")
        :|8_Worksite_16x16|
        (:|source| "limezu -- https://limezu.itch.io/" :|frameConfig|
         (:|columns| 32 :|tilecount| 640 :|imagewidth| 512 :|imageheight| 320
          :|spacing| 0 :|margin| 0 :|frameHeight| 16 :|frameWidth| 16)
         :|animLength| -1 :|key| "8_Worksite_16x16")
        :|7_Villas_16x16|
        (:|source| "limezu -- https://limezu.itch.io/" :|frameConfig|
         (:|columns| 32 :|tilecount| 1824 :|imagewidth| 512 :|imageheight| 912
          :|spacing| 0 :|margin| 0 :|frameHeight| 16 :|frameWidth| 16)
         :|animLength| -1 :|key| "7_Villas_16x16")
        :|6_Garage_Sales_16x16|
        (:|source| "limezu -- https://limezu.itch.io/" :|frameConfig|
         (:|columns| 32 :|tilecount| 1216 :|imagewidth| 512 :|imageheight| 608
          :|spacing| 0 :|margin| 0 :|frameHeight| 16 :|frameWidth| 16)
         :|animLength| -1 :|key| "6_Garage_Sales_16x16")
        :|5_Floor_Modular_Buildings_16x16|
        (:|source| "limezu -- https://limezu.itch.io/" :|frameConfig|
         (:|columns| 32 :|tilecount| 8288 :|imagewidth| 512 :|imageheight| 4144
          :|spacing| 0 :|margin| 0 :|frameHeight| 16 :|frameWidth| 16)
         :|animLength| -1 :|key| "5_Floor_Modular_Buildings_16x16")
        :|4_Generic_Buildings_16x16|
        (:|source| "limezu -- https://limezu.itch.io/" :|frameConfig|
         (:|columns| 32 :|tilecount| 3712 :|imagewidth| 512 :|imageheight| 1856
          :|spacing| 0 :|margin| 0 :|frameHeight| 16 :|frameWidth| 16)
         :|animLength| -1 :|key| "4_Generic_Buildings_16x16")
        :|3_City_Props_16x16|
        (:|source| "limezu -- https://limezu.itch.io/" :|frameConfig|
         (:|columns| 32 :|tilecount| 1824 :|imagewidth| 512 :|imageheight| 912
          :|spacing| 0 :|margin| 0 :|frameHeight| 16 :|frameWidth| 16)
         :|animLength| -1 :|key| "3_City_Props_16x16")
        :|2_City_Terrains_16x16|
        (:|source| "limezu -- https://limezu.itch.io/" :|frameConfig|
         (:|columns| 32 :|tilecount| 2144 :|imagewidth| 512 :|imageheight| 1072
          :|spacing| 0 :|margin| 0 :|frameHeight| 16 :|frameWidth| 16)
         :|animLength| -1 :|key| "2_City_Terrains_16x16")
        :|1_Terrains_and_Fences_16x16|
        (:|source| "limezu -- https://limezu.itch.io/" :|frameConfig|
         (:|columns| 32 :|tilecount| 1312 :|imagewidth| 512 :|imageheight| 656
          :|spacing| 0 :|margin| 0 :|frameHeight| 16 :|frameWidth| 16)
         :|animLength| -1 :|key| "1_Terrains_and_Fences_16x16")
                                        ; :|concrete|
        ;; (:|source| "original -- original" :|frameConfig|
        ;;  (:|columns| 8 :|tilecount| 40 :|imagewidth| 128 :|imageheight| 85 :|spacing|
        ;;   2 :|margin| 1 :|frameHeight| 16 :|frameWidth| 16)
        ;;  :|animLength| -1 :|url| "assets/images/concrete.png" :|key| "concrete")
        :|room_1|
        (:|source| "limezu -- https://limezu.itch.io/" :|frameConfig|
         (:|columns| 0 :|tilecount| 0 :|imagewidth| 0 :|imageheight| 0 :|spacing| 0
                       :|margin| 0 :|frameHeight| 16 :|frameWidth| 16)
         :|animLength| -1 :|url| "assets/images/room_1.png" :|key| "Room_1")
        :|9_Fishing_16x16|
        (:|source| "limezu -- https://limezu.itch.io/" :|frameConfig|
         (:|columns| 16 :|tilecount| 432 :|imagewidth| 256 :|imageheight| 432
          :|spacing| 0 :|margin| 0 :|frameHeight| 16 :|frameWidth| 16)
         :|animLength| -1 :|url| "assets/images/9_Fishing_16x16.png" :|key|
         "9_Fishing_16x16")
        :|8_Gym_16x16|
        (:|source| "limezu -- https://limezu.itch.io/" :|frameConfig|
         (:|columns| 16 :|tilecount| 528 :|imagewidth| 256 :|imageheight| 528
          :|spacing| 0 :|margin| 0 :|frameHeight| 16 :|frameWidth| 16)
         :|animLength| -1 :|url| "assets/images/8_Gym_16x16.png" :|key|
         "8_Gym_16x16")
        :|7_Art_16x16|
        (:|source| "limezu -- https://limezu.itch.io/" :|frameConfig|
         (:|columns| 16 :|tilecount| 112 :|imagewidth| 256 :|imageheight| 112
          :|spacing| 0 :|margin| 0 :|frameHeight| 16 :|frameWidth| 16)
         :|animLength| -1 :|url| "assets/images/7_Art_16x16.png" :|key|
         "7_Art_16x16")
        :|6_Music_and_sport_16x16|
        (:|source| "limezu -- https://limezu.itch.io/" :|frameConfig|
         (:|columns| 16 :|tilecount| 768 :|imagewidth| 256 :|imageheight| 768
          :|spacing| 0 :|margin| 0 :|frameHeight| 16 :|frameWidth| 16)
         :|animLength| -1 :|url| "assets/images/6_Music_and_sport_16x16.png" :|key|
         "6_Music_and_sport_16x16")
        :|5_Classroom_and_library_16x16|
        (:|source| "limezu -- https://limezu.itch.io/" :|frameConfig|
         (:|columns| 16 :|tilecount| 544 :|imagewidth| 256 :|imageheight| 544
          :|spacing| 0 :|margin| 0 :|frameHeight| 16 :|frameWidth| 16)
         :|animLength| -1 :|url| "assets/images/5_Classroom_and_library_16x16.png"
         :|key| "5_Classroom_and_library_16x16")
        :|4_Bedroom_16x16|
        (:|source| "limezu -- https://limezu.itch.io/" :|frameConfig|
         (:|columns| 16 :|tilecount| 1712 :|imagewidth| 256 :|imageheight| 1712
          :|spacing| 0 :|margin| 0 :|frameHeight| 16 :|frameWidth| 16)
         :|animLength| -1 :|url| "assets/images/4_Bedroom_16x16.png" :|key|
         "4_Bedroom_16x16")
        :|3_Bathroom_16x16|
        (:|source| "limezu -- https://limezu.itch.io/" :|frameConfig|
         (:|columns| 16 :|tilecount| 896 :|imagewidth| 256 :|imageheight| 896
          :|spacing| 0 :|margin| 0 :|frameHeight| 16 :|frameWidth| 16)
         :|animLength| -1 :|url| "assets/images/3_Bathroom_16x16.png" :|key|
         "3_Bathroom_16x16")
        :|2_LivingRoom_16x16|
        (:|source| "limezu -- https://limezu.itch.io/" :|frameConfig|
         (:|columns| 16 :|tilecount| 720 :|imagewidth| 256 :|imageheight| 720
          :|spacing| 0 :|margin| 0 :|frameHeight| 16 :|frameWidth| 16)
         :|animLength| -1 :|url| "assets/images/2_LivingRoom_16x16.png" :|key|
         "2_LivingRoom_16x16")
        :|26_Condominium|
        (:|source| "limezu -- https://limezu.itch.io/" :|frameConfig|
         (:|columns| 16 :|tilecount| 304 :|imagewidth| 256 :|imageheight| 304
          :|spacing| 0 :|margin| 0 :|frameHeight| 16 :|frameWidth| 16)
         :|animLength| -1 :|url| "assets/images/26_Condominium.png" :|key|
         "26_Condominium")
        :|25_Shooting_Range|
        (:|source| "limezu -- https://limezu.itch.io/" :|frameConfig|
         (:|columns| 16 :|tilecount| 80 :|imagewidth| 256 :|imageheight| 80
          :|spacing| 0 :|margin| 0 :|frameHeight| 16 :|frameWidth| 16)
         :|animLength| -1 :|url| "assets/images/25_Shooting_Range.png" :|key|
         "25_Shooting_Range")
        :|24_Ice_Cream_Shop|
        (:|source| "limezu -- https://limezu.itch.io/" :|frameConfig|
         (:|columns| 16 :|tilecount| 272 :|imagewidth| 256 :|imageheight| 272
          :|spacing| 0 :|margin| 0 :|frameHeight| 16 :|frameWidth| 16)
         :|animLength| -1 :|url| "assets/images/24_Ice_Cream_Shop.png" :|key|
         "24_Ice_Cream_Shop")
        :|23_Tevelision_and_Film_Studio|
        (:|source| "limezu -- https://limezu.itch.io/" :|frameConfig|
         (:|columns| 16 :|tilecount| 224 :|imagewidth| 256 :|imageheight| 224
          :|spacing| 0 :|margin| 0 :|frameHeight| 16 :|frameWidth| 16)
         :|animLength| -1 :|url| "assets/images/23_Tevelision_and_Film_Studio.png"
         :|key| "23_Tevelision_and_Film_Studio")
        :|22_Museum|
        (:|source| "limezu -- https://limezu.itch.io/" :|frameConfig|
         (:|columns| 16 :|tilecount| 1952 :|imagewidth| 256 :|imageheight| 1952
          :|spacing| 0 :|margin| 0 :|frameHeight| 16 :|frameWidth| 16)
         :|animLength| -1 :|url| "assets/images/22_Museum.png" :|key| "22_Museum")
        :|21_Clothing_Store|
        (:|source| "limezu -- https://limezu.itch.io/" :|frameConfig|
         (:|columns| 16 :|tilecount| 1072 :|imagewidth| 256 :|imageheight| 1072
          :|spacing| 0 :|margin| 0 :|frameHeight| 16 :|frameWidth| 16)
         :|animLength| -1 :|url| "assets/images/21_Clothing_Store.png" :|key|
         "21_Clothing_Store")
        :|20_Japanese_interiors|
        (:|source| "limezu -- https://limezu.itch.io/" :|frameConfig|
         (:|columns| 16 :|tilecount| 512 :|imagewidth| 256 :|imageheight| 512
          :|spacing| 0 :|margin| 0 :|frameHeight| 16 :|frameWidth| 16)
         :|animLength| -1 :|url| "assets/images/20_Japanese_interiors.png" :|key|
         "20_Japanese_interiors")
        :|1_Generic_16x16|
        (:|source| "limezu -- https://limezu.itch.io/" :|frameConfig|
         (:|columns| 16 :|tilecount| 1248 :|imagewidth| 256 :|imageheight| 1248
          :|spacing| 0 :|margin| 0 :|frameHeight| 16 :|frameWidth| 16)
         :|animLength| -1 :|url| "assets/images/1_Generic_16x16.png" :|key|
         "1_Generic_16x16")
        :|19_Hospital_16x16|
        (:|source| "limezu -- https://limezu.itch.io/" :|frameConfig|
         (:|columns| 16 :|tilecount| 1760 :|imagewidth| 256 :|imageheight| 1760
          :|spacing| 0 :|margin| 0 :|frameHeight| 16 :|frameWidth| 16)
         :|animLength| -1 :|url| "assets/images/19_Hospital_16x16.png" :|key|
         "19_Hospital_16x16")
        :|18_Jail_16x16|
        (:|source| "limezu -- https://limezu.itch.io/" :|frameConfig|
         (:|columns| 16 :|tilecount| 720 :|imagewidth| 256 :|imageheight| 720
          :|spacing| 0 :|margin| 0 :|frameHeight| 16 :|frameWidth| 16)
         :|animLength| -1 :|url| "assets/images/18_Jail_16x16.png" :|key|
         "18_Jail_16x16")
        :|17_Visibile_Upstairs_System_16x16|
        (:|source| "limezu -- https://limezu.itch.io/" :|frameConfig|
         (:|columns| 16 :|tilecount| 432 :|imagewidth| 256 :|imageheight| 432
          :|spacing| 0 :|margin| 0 :|frameHeight| 16 :|frameWidth| 16)
         :|animLength| -1 :|url|
         "assets/images/17_Visibile_Upstairs_System_16x16.png" :|key|
         "17_Visibile_Upstairs_System_16x16")
        :|16_Grocery_store_16x16|
        (:|source| "limezu -- https://limezu.itch.io/" :|frameConfig|
         (:|columns| 16 :|tilecount| 1248 :|imagewidth| 256 :|imageheight| 1248
          :|spacing| 0 :|margin| 0 :|frameHeight| 16 :|frameWidth| 16)
         :|animLength| -1 :|url| "assets/images/16_Grocery_store_16x16.png" :|key|
         "16_Grocery_store_16x16")
        :|15_Christmas_16x16|
        (:|source| "limezu -- https://limezu.itch.io/" :|frameConfig|
         (:|columns| 16 :|tilecount| 272 :|imagewidth| 256 :|imageheight| 272
          :|spacing| 0 :|margin| 0 :|frameHeight| 16 :|frameWidth| 16)
         :|animLength| -1 :|url| "assets/images/15_Christmas_16x16.png" :|key|
         "15_Christmas_16x16")
        :|14_Basement_16x16|
        (:|source| "limezu -- https://limezu.itch.io/" :|frameConfig|
         (:|columns| 16 :|tilecount| 800 :|imagewidth| 256 :|imageheight| 800
          :|spacing| 0 :|margin| 0 :|frameHeight| 16 :|frameWidth| 16)
         :|animLength| -1 :|url| "assets/images/14_Basement_16x16.png" :|key|
         "14_Basement_16x16")
        :|13_Conference_Hall_16x16|
        (:|source| "limezu -- https://limezu.itch.io/" :|frameConfig|
         (:|columns| 16 :|tilecount| 192 :|imagewidth| 256 :|imageheight| 192
          :|spacing| 0 :|margin| 0 :|frameHeight| 16 :|frameWidth| 16)
         :|animLength| -1 :|url| "assets/images/13_Conference_Hall_16x16.png" :|key|
         "13_Conference_Hall_16x16")
        :|12_Kitchen_16x16|
        (:|source| "limezu -- https://limezu.itch.io/" :|frameConfig|
         (:|columns| 16 :|tilecount| 784 :|imagewidth| 256 :|imageheight| 784
          :|spacing| 0 :|margin| 0 :|frameHeight| 16 :|frameWidth| 16)
         :|animLength| -1 :|url| "assets/images/12_Kitchen_16x16.png" :|key|
         "12_Kitchen_16x16")
        :|11_Halloween_16x16|
        (:|source| "limezu -- https://limezu.itch.io/" :|frameConfig|
         (:|columns| 16 :|tilecount| 976 :|imagewidth| 256 :|imageheight| 976
          :|spacing| 0 :|margin| 0 :|frameHeight| 16 :|frameWidth| 16)
         :|animLength| -1 :|url| "assets/images/11_Halloween_16x16.png" :|key|
         "11_Halloween_16x16")
        :|10_Birthday_party_16x16|
        (:|source| "limezu -- https://limezu.itch.io/" :|frameConfig|
         (:|columns| 12 :|tilecount| 84 :|imagewidth| 192 :|imageheight| 112
          :|spacing| 0 :|margin| 0 :|frameHeight| 16 :|frameWidth| 16)
         :|animLength| -1 :|url| "assets/images/10_Birthday_party_16x16.png" :|key|
         "10_Birthday_party_16x16")
        :|leaves|
        (:|source| "original" :|frameConfig|
         (:|columns| 4 :|tilecount| 12 :|imagewidth| 32 :|imageheight| 24 :|spacing|
          0 :|margin| 0 :|frameHeight| 8 :|frameWidth| 8)
         :|animLength| 12 :|url| "assets/images/leaves.png" :|key| "leaves")
        :|hyptosis-tileset-16-extrude-sheet|
        (:|source| "hyptosis -- https://hyptosis.newgrounds.com/" :|frameConfig|
         (:|columns| 60 :|tilecount| 3720 :|imagewidth| 1080 :|imageheight| 1116
          :|spacing| 2 :|margin| 1 :|frameHeight| 16 :|frameWidth| 16)
         :|animLength| -1 :|url|
         "assets/images/hyptosis-tileset-16-extrude-sheet.png" :|key|
         "hyptosis-tileset-16-extrude-sheet")
        :|roguelikeCity_transparent|
        (:|source| "kenney -- kenney.nl" :|frameConfig|
         (:|columns| 37 :|tilecount| 1036 :|imagewidth| 666 :|imageheight| 504
          :|spacing| 2 :|margin| 1 :|frameHeight| 16 :|frameWidth| 16)
         :|animLength| -1 :|url| "assets/images/roguelikeCity_transparent.png" :|key|
         "roguelikeCity_transparent")
        :|roguelikeDungeon_transparent|
        (:|source| "kenney -- kenney.nl" :|frameConfig|
         (:|columns| 29 :|tilecount| 522 :|imagewidth| 522 :|imageheight| 324
          :|spacing| 2 :|margin| 1 :|frameHeight| 16 :|frameWidth| 16)
         :|animLength| -1 :|url| "assets/images/roguelikeDungeon_transparent.png"
         :|key| "roguelikeDungeon_transparent")
        :|roguelikeIndoor_transparent|
        (:|source| "kenney -- kenney.nl" :|frameConfig|
         (:|columns| 26 :|tilecount| 442 :|imagewidth| 458 :|imageheight| 305
          :|spacing| 2 :|margin| 1 :|frameHeight| 16 :|frameWidth| 16)
         :|animLength| -1 :|url| "assets/images/roguelikeIndoor_transparent.png"
         :|key| "roguelikeIndoor_transparent")
        :|desert_walls|
        (:|source| "original -- original" :|frameConfig|
         (:|columns| 0 :|tilecount| 0 :|imagewidth| 0 :|imageheight| 0 :|spacing| nil
                       :|margin| nil :|frameHeight| nil :|frameWidth| nil)
         :|url| nil :|key| "desert_walls")
        :|desert_building-sheet|
        (:|source| "Rob Sneed -- https://www.flickr.com/photos/57146521@N02/"
         :|frameConfig|
         (:|columns| 0 :|tilecount| 0 :|imagewidth| 0 :|imageheight| 0 :|spacing| 2
                       :|margin| 1 :|frameHeight| 16 :|frameWidth| 16)
         :|url| "assets/images/desert_building-sheet.png" :|key|
         "desert_building-sheet")
        :|wang_template|
        (:|source| "caeles -- https://opengameart.org/users/caeles" :|frameConfig|
         (:|columns| 16 :|tilecount| 256 :|imagewidth| 256 :|imageheight| 256
          :|spacing| 0 :|margin| 0 :|frameHeight| 16 :|frameWidth| 16)
         :|url| "assets/images/wang_template.png" :|key| "wang_template")
        :|constructionman_sprite|
        (:|source| "textfiles.com -- http://www.textfiles.com/underconstruction/"
         :|frameConfig|
         (:|columns| 3 :|tilecount| 3 :|imagewidth| 243 :|imageheight| 50 :|spacing|
          nil :|margin| nil :|frameHeight| 50 :|frameWidth| 81)
         :|animLength| 3 :|url| "assets/images/constructionman_sprite.png" :|key|
         "constructionman_sprite")
        :|scut_extrude-city-16-sheet|
        (:|source| nil :|frameConfig|
         (:|columns| 0 :|tilecount| 0 :|imagewidth| 0 :|imageheight| 0 :|spacing| 2
                       :|margin| 1 :|frameHeight| 16 :|frameWidth| 16)
         :|url| "assets/images/scut_city_extrude-16-sheet.png" :|key|
         "scut_extrude-city-16-sheet")
        :|Cards_Diamonds|
        (:|source| "Chasersgaming -- https://chasersgaming.itch.io/" :|frameConfig|
         (:|columns| 6 :|tilecount| 18 :|imagewidth| 256 :|imageheight| 192
          :|spacing| 14 :|margin| 7 :|frameHeight| 50 :|frameWidth| 34)
         :|url| "assets/images/Cards_Diamonds.png" :|key| "Cards_Diamonds")
        :|Cards_Hearts|
        (:|source| "Chasersgaming -- https://chasersgaming.itch.io/" :|frameConfig|
         (:|columns| 6 :|tilecount| 18 :|imagewidth| 256 :|imageheight| 192
          :|spacing| 14 :|margin| 7 :|frameHeight| 50 :|frameWidth| 34)
         :|url| "assets/images/Cards_Hearts.png" :|key| "Cards_Hearts")
        :|Cards_Spades|
        (:|source| "Chasersgaming -- https://chasersgaming.itch.io/" :|frameConfig|
         (:|columns| 6 :|tilecount| 18 :|imagewidth| 256 :|imageheight| 192
          :|spacing| 14 :|margin| 7 :|frameHeight| 50 :|frameWidth| 34)
         :|url| "assets/images/Cards_Spades.png" :|key| "Cards_Spades")
        :|Cards_Clubs|
        (:|source| "Chasersgaming -- https://chasersgaming.itch.io/" :|frameConfig|
         (:|columns| 6 :|tilecount| 18 :|imagewidth| 256 :|imageheight| 192
          :|spacing| 14 :|margin| 7 :|frameHeight| 50 :|frameWidth| 34)
         :|url| "assets/images/Cards_Clubs.png" :|key| "Cards_Clubs")
        :|desert-cliffs-extrude|
        (:|source| " -- " :|frameConfig|
         (:|columns| 7 :|tilecount| 63 :|imagewidth| 126 :|imageheight| 162
          :|spacing| 2 :|margin| 1 :|frameHeight| 16 :|frameWidth| 16)
         :|animLength| -1 :|url| "assets/images/desert-cliffs-extrude.png" :|key|
         "desert-cliffs-extrude")
        :|browserquestextrude|
        (:|source| " -- " :|frameConfig|
         (:|columns| 20 :|tilecount| 1960 :|imagewidth| 360 :|imageheight| 1764
          :|spacing| 2 :|margin| 1 :|frameHeight| 16 :|frameWidth| 16)
         :|animLength| -1 :|url| "assets/images/browserquestextrude.png" :|key|
         "browserquestextrude")
        :|browserquest|
        (:|source| " -- " :|frameConfig|
         (:|columns| 20 :|tilecount| 1568 :|imagewidth| 320 :|imageheight| 1764
          :|spacing| 0 :|margin| 0 :|frameHeight| 16 :|frameWidth| 16)
         :|animLength| -1 :|url| "assets/images/browserquest.png" :|key|
         "browserquest")
        :|scut_extrude-16|
        (:|source| "scut -- https://scut.itch.io" :|frameConfig|
         (:|columns| 5 :|tilecount| 120 :|imagewidth| 90 :|imageheight| 432
          :|spacing| 2 :|margin| 1 :|frameHeight| 16 :|frameWidth| 16)
         :|animLength| -1 :|url| "assets/images/scut_extrude-16.png" :|key|
         "scut_extrude-16")
        :|animals4|
        (:|source| "finalbossblues -- http://www.timefantasy.net/" :|frameConfig|
         (:|columns| 12 :|tilecount| 96 :|imagewidth| 504 :|imageheight| 288
          :|spacing| nil :|margin| nil :|frameHeight| 36 :|frameWidth| 42)
         :|animLength| 3 :|url| "assets/images/animals4.png" :|key| "animals4")
        :|animals3|
        (:|source| "finalbossblues -- http://www.timefantasy.net/" :|frameConfig|
         (:|columns| 12 :|tilecount| 96 :|imagewidth| 504 :|imageheight| 288
          :|spacing| nil :|margin| nil :|frameHeight| 36 :|frameWidth| 42)
         :|animLength| 3 :|url| "assets/images/animals3.png" :|key| "animals3")
        :|animals2|
        (:|source| "finalbossblues -- http://www.timefantasy.net/" :|frameConfig|
         (:|columns| 12 :|tilecount| 96 :|imagewidth| 504 :|imageheight| 288
          :|spacing| nil :|margin| nil :|frameHeight| 36 :|frameWidth| 42)
         :|animLength| 3 :|url| "assets/images/animals2.png" :|key| "animals2")
        :|animals1|
        (:|source| "finalbossblues -- http://www.timefantasy.net/" :|frameConfig|
         (:|columns| 12 :|tilecount| 96 :|imagewidth| 312 :|imageheight| 288
          :|spacing| nil :|margin| nil :|frameHeight| 36 :|frameWidth| 26)
         :|animLength| 3 :|url| "assets/images/animals1.png" :|key| "animals1")
        :|studentmale|
        (:|source| "pipoya -- https://pipoya.itch.io/" :|frameConfig|
         (:|columns| 3 :|tilecount| 12 :|imagewidth| 96 :|imageheight| 128 :|spacing|
          nil :|margin| nil :|frameHeight| 32 :|frameWidth| 32)
         :|animLength| 3 :|url| "assets/images/studentmale.png" :|key| "studentmale")
        :|small-turtles|
        (:|source| "whtdragons -- https://opengameart.org/users/whtdragons"
         :|frameConfig|
         (:|columns| 12 :|tilecount| 96 :|imagewidth| 252 :|imageheight| 112
          :|spacing| nil :|margin| nil :|frameHeight| 14 :|frameWidth| 21)
         :|animLength| 3 :|url| "assets/images/small-turtles.png" :|key|
         "small-turtles")
        :|workers|
        (:|source| "SdiviHall -- https://opengameart.org/users/sdivihall"
         :|frameConfig|
         (:|columns| 12 :|tilecount| 96 :|imagewidth| 192 :|imageheight| 128
          :|spacing| nil :|margin| nil :|frameHeight| 16 :|frameWidth| 16)
         :|animLength| 3 :|url| "assets/images/workers.png" :|key| "workers")
        :|circle_anim_16px|
        (:|source| "original" :|frameConfig|
         (:|columns| 7 :|tilecount| 7 :|imagewidth| 108 :|imageheight| 18 :|spacing|
          1 :|margin| 2 :|frameHeight| 16 :|frameWidth| 16)
         :|animLength| 6 :|url| "assets/images/circle_anim_16px.png" :|key|
         "circle_anim_16px")
        :|iching|
        (:|source| "unknown -- unknown" :|frameConfig|
         (:|columns| 9 :|tilecount| 81 :|imagewidth| 679 :|imageheight| 676
          :|spacing| nil :|margin| 8 :|frameHeight| 82 :|frameWidth| 82)
         :|animLength| -1 :|url| "assets/images/iching.png" :|key| "iching")
        :|ghost1|
        (:|source| "pipoya -- https://pipoya.itch.io/" :|frameConfig|
         (:|columns| 3 :|tilecount| 12 :|imagewidth| 96 :|imageheight| 128 :|spacing|
          nil :|margin| nil :|frameHeight| 32 :|frameWidth| 32)
         :|animLength| 3 :|url| "assets/images/ghost1.png" :|key| "ghost1")
        :|male102|
        (:|source| "pipoya -- https://pipoya.itch.io/" :|frameConfig|
         (:|columns| 3 :|tilecount| 12 :|imagewidth| 96 :|imageheight| 128 :|spacing|
          nil :|margin| nil :|frameHeight| 32 :|frameWidth| 32)
         :|animLength| 3 :|url| "assets/images/male102.png" :|key| "male102")
        :|NPC_test|
        (:|source| "ArMM1998 -- https://opengameart.org/users/armm1998"
         :|frameConfig|
         (:|columns| 4 :|tilecount| 16 :|imagewidth| 64 :|imageheight| 128 :|spacing|
          nil :|margin| nil :|frameHeight| 32 :|frameWidth| 16)
         :|animLength| 4 :|url| "assets/images/NPC_test.png" :|key| "NPC_test")
        :|Musical_staff|
        (:|source|
         "WikiCommons -- https://commons.wikimedia.org/wiki/File:Musical_staff.png"
         :|url| "assets/images/Musical_staff.png" :|key| "Musical_staff")
        :|default|
        (:|source| "original -- original" :|url| "assets/images/default.png" :|key|
         "default")
        :|RainForestCanopy6678constructani|
        (:|source| "textfiles.com -- http://www.textfiles.com/underconstruction/"
         :|url| "assets/images/RainForestCanopy6678constructani.gif" :|key|
         "RainForestCanopy6678constructani")
        :|constructionman|
        (:|source| "textfiles.com -- textfiles.com" :|url|
         "assets/images/constructionman.gif" :|key| "constructionman")
        :|construction|
        (:|source| "textfiles.com -- textfiles.com" :|url|
         "assets/images/construction.gif" :|key| "construction")
        :|city_aerial2|
        (:|source| "Murray Foubister -- https://www.flickr.com/photos/mfoubister/"
         :|url| "assets/images/city_aerial2.png" :|key| "cityaerial2")
        :|city_aerial1|
        (:|source| "Murray Foubister -- https://www.flickr.com/photos/mfoubister/"
         :|url| "assets/images/city_aerial1.png" :|key| "cityaerial1")
        :|treeheads1|
        (:|source| "unknown -- unknown" :|url| "assets/images/treeheads1.png" :|key|
         "treeheads1")
        :|trash|
        (:|source| "unknown -- unknown" :|url| "assets/images/trash.png" :|key|
         "trash")
        :|babel-painting|
        (:|source|
         " -- https://institutopoimenica.files.wordpress.com/2016/02/tower-of-babel_momper_francken-large1.jpg,"
         :|url| "assets/images/babel.png" :|key| "babel-painting")
        :|squirrel-painting|
        (:|source|
         " -- https://artsandculture.google.com/asset/squirrel-katsuta-shokin/8QELQp7u8LXBrQ,"
         :|url| "assets/images/squirrel-painting.png" :|key| "squirrel-painting")
        :|dog-portrait|
        (:|source|
         " -- https://artsandculture.google.com/asset/the-painter-and-his-pug-william-hogarth/1AHBHUMVSfHpFg"
         :|url| "assets/images/dog-portrait.png" :|key| "dog-portrait")
        :|vanmour|
        (:|source|
         "https://www.peramuseum.org/artwork/the-ambassadorial-procession/15/203"
         :|url| "assets/images/vanmour.png" :|key| "vanmour")
        :|editors_meeting|
        (:|source| "Michael Coghlan -- https://www.flickr.com/photos/mikecogh/"
         :|url| "assets/images/editors_meeting.jpg" :|key| "editors_meeting")
        :|drone_river|
        (:|source| "unknown -- unknown" :|url| "assets/images/drone_river.jpg" :|key|
         "drone_river")
        :|toilet|
        (:|source| "unknown -- unknown" :|url| "assets/images/toilet.png" :|key|
         "toilet")
        :|canyon3|
        (:|source| "unknown -- unknown" :|url| "assets/images/canyon3.png" :|key|
         "canyon3")
        :|canyon2|
        (:|source| "unknown -- unknown" :|url| "assets/images/canyon2.png" :|key|
         "canyon2")
        :|canyon|
        (:|source| "unknown -- unknown" :|url| "assets/images/canyon.png" :|key|
         "canyon")
        :|hormuz_oli_2020267_lrg|
        (:|source| "unknown" :|url| "assets/images/hormuz_oli_2020267_lrg.png" :|key|
         "hormuz_oli_2020267_lrg")))

(setq characters
      '(:|zebra|
        (:|anims|
         (:|west| (3 4 5) :|east| (6 7 8) :|south| (0 1 2) :|north| (9 10 11))
         :|texture| "tf_zebra" :|name| "zebra")
        :|walrus|
        (:|anims|
         (:|west| (3 4 5) :|east| (6 7 8) :|south| (0 1 2) :|north| (9 10 11))
         :|texture| "tf_walrus" :|name| "walrus")
        :|vulture_sit|
        (:|anims|
         (:|west| (3 4 5) :|east| (6 7 8) :|south| (0 1 2) :|north| (9 10 11))
         :|texture| "tf_vulture_sit" :|name| "vulture_sit")
        :|vulture_fly|
        (:|anims|
         (:|west| (3 4 5) :|east| (6 7 8) :|south| (0 1 2) :|north| (9 10 11))
         :|texture| "tf_vulture_fly" :|name| "vulture_fly")
        ;; :|turtle|
        ;; (:|anims|
        ;;  (:|west| (3 4 5) :|east| (6 7 8) :|south| (0 1 2) :|north| (9 10 11))
        ;;  :|texture| "tf_turtle" :|name| "turtle")
        :|toucan_sit|
        (:|anims|
         (:|west| (3 4 5) :|east| (6 7 8) :|south| (0 1 2) :|north| (9 10 11))
         :|texture| "tf_toucan_sit" :|name| "toucan_sit")
        :|toucan_fly|
        (:|anims|
         (:|west| (3 4 5) :|east| (6 7 8) :|south| (0 1 2) :|north| (9 10 11))
         :|texture| "tf_toucan_fly" :|name| "toucan_fly")
        :|tiger_cub|
        (:|anims|
         (:|west| (3 4 5) :|east| (6 7 8) :|south| (0 1 2) :|north| (9 10 11))
         :|texture| "tf_tiger_cub" :|name| "tiger_cub")
        :|tiger|
        (:|anims|
         (:|west| (3 4 5) :|east| (6 7 8) :|south| (0 1 2) :|north| (9 10 11))
         :|texture| "tf_tiger" :|name| "tiger")
        ;; :|swordfish|
        ;; (:|anims|
        ;;  (:|west| (3 4 5) :|east| (6 7 8) :|south| (0 1 2) :|north| (9 10 11))
        ;;  :|texture| "tf_swordfish" :|name| "swordfish")
        ;; :|shark_full_brown_1|
        ;; (:|anims|
        ;;  (:|west| (3 4 5) :|east| (6 7 8) :|south| (0 1 2) :|north| (9 10 11))
        ;;  :|texture| "tf_shark_full_brown_1" :|name| "shark_full_brown_1")
        ;; :|shark_full_blue_1|
        ;; (:|anims|
        ;;  (:|west| (3 4 5) :|east| (6 7 8) :|south| (0 1 2) :|north| (9 10 11))
        ;;  :|texture| "tf_shark_full_blue_1" :|name| "shark_full_blue_1")
        ;; :|shark_fin_brown_1|
        ;; (:|anims|
        ;;  (:|west| (3 4 5) :|east| (6 7 8) :|south| (0 1 2) :|north| (9 10 11))
        ;;  :|texture| "tf_shark_fin_brown_1" :|name| "shark_fin_brown_1")
        ;; :|shark_fin_blue_1|
        ;; (:|anims|
        ;;  (:|west| (3 4 5) :|east| (6 7 8) :|south| (0 1 2) :|north| (9 10 11))
        ;;  :|texture| "tf_shark_fin_blue_1" :|name| "shark_fin_blue_1")
        ;; :|shark_finshadow_brown_1|
        ;; (:|anims|
        ;;  (:|west| (3 4 5) :|east| (6 7 8) :|south| (0 1 2) :|north| (9 10 11))
        ;;  :|texture| "tf_shark_finshadow_brown_1" :|name| "shark_finshadow_brown_1")
        ;; :|shark_finshadow_blue_1|
        ;; (:|anims|
        ;;  (:|west| (3 4 5) :|east| (6 7 8) :|south| (0 1 2) :|north| (9 10 11))
        ;;  :|texture| "tf_shark_finshadow_blue_1" :|name| "shark_finshadow_blue_1")
        :|seaturtle|
        (:|anims|
         (:|west| (3 4 5) :|east| (6 7 8) :|south| (0 1 2) :|north| (9 10 11))
         :|texture| "tf_seaturtle" :|name| "seaturtle")
        :|seal|
        (:|anims|
         (:|west| (3 4 5) :|east| (6 7 8) :|south| (0 1 2) :|north| (9 10 11))
         :|texture| "tf_seal" :|name| "seal")
        :|sabretooth|
        (:|anims|
         (:|west| (3 4 5) :|east| (6 7 8) :|south| (0 1 2) :|north| (9 10 11))
         :|texture| "tf_sabretooth" :|name| "sabretooth")
        :|rhinoceros|
        (:|anims|
         (:|west| (3 4 5) :|east| (6 7 8) :|south| (0 1 2) :|north| (9 10 11))
         :|texture| "tf_rhinoceros" :|name| "rhinoceros")
        ;; :|ray|
        ;; (:|anims|
        ;;  (:|west| (3 4 5) :|east| (6 7 8) :|south| (0 1 2) :|north| (9 10 11))
        ;;  :|texture| "tf_ray" :|name| "ray")
        ;; :|pufferfish_small|
        ;; (:|anims|
        ;;  (:|west| (3 4 5) :|east| (6 7 8) :|south| (0 1 2) :|north| (9 10 11))
        ;;  :|texture| "tf_pufferfish_small" :|name| "pufferfish_small")
        ;; :|pufferfish_big|
        ;; (:|anims|
        ;;  (:|west| (3 4 5) :|east| (6 7 8) :|south| (0 1 2) :|north| (9 10 11))
        ;;  :|texture| "tf_pufferfish_big" :|name| "pufferfish_big")
        :|penguin_baby|
        (:|anims|
         (:|west| (3 4 5) :|east| (6 7 8) :|south| (0 1 2) :|north| (9 10 11))
         :|texture| "tf_penguin_baby" :|name| "penguin_baby")
        :|penguin|
        (:|anims|
         (:|west| (3 4 5) :|east| (6 7 8) :|south| (0 1 2) :|north| (9 10 11))
         :|texture| "tf_penguin" :|name| "penguin")
        :|parrot_sit|
        (:|anims|
         (:|west| (3 4 5) :|east| (6 7 8) :|south| (0 1 2) :|north| (9 10 11))
         :|texture| "tf_parrot_sit" :|name| "parrot_sit")
        :|parrot_fly|
        (:|anims|
         (:|west| (3 4 5) :|east| (6 7 8) :|south| (0 1 2) :|north| (9 10 11))
         :|texture| "tf_parrot_fly" :|name| "parrot_fly")
        :|panther_cub|
        (:|anims|
         (:|west| (3 4 5) :|east| (6 7 8) :|south| (0 1 2) :|north| (9 10 11))
         :|texture| "tf_panther_cub" :|name| "panther_cub")
        :|panther|
        (:|anims|
         (:|west| (3 4 5) :|east| (6 7 8) :|south| (0 1 2) :|north| (9 10 11))
         :|texture| "tf_panther" :|name| "panther")
        :|owl_sit|
        (:|anims|
         (:|west| (3 4 5) :|east| (6 7 8) :|south| (0 1 2) :|north| (9 10 11))
         :|texture| "tf_owl_sit" :|name| "owl_sit")
        :|owl_fly|
        (:|anims|
         (:|west| (3 4 5) :|east| (6 7 8) :|south| (0 1 2) :|north| (9 10 11))
         :|texture| "tf_owl_fly" :|name| "owl_fly")
        ;; :|monkey_8sheet|
        ;; (:|anims|
        ;;  (:|west| (3 4 5) :|east| (6 7 8) :|south| (0 1 2) :|north| (9 10 11))
        ;;  :|texture| "tf_monkey_8sheet" :|name| "monkey_8sheet")
        :|mammoth|
        (:|anims|
         (:|west| (3 4 5) :|east| (6 7 8) :|south| (0 1 2) :|north| (9 10 11))
         :|texture| "tf_mammoth" :|name| "mammoth")
        :|lizard|
        (:|anims|
         (:|west| (3 4 5) :|east| (6 7 8) :|south| (0 1 2) :|north| (9 10 11))
         :|texture| "tf_lizard" :|name| "lizard")
        :|lion_cub|
        (:|anims|
         (:|west| (3 4 5) :|east| (6 7 8) :|south| (0 1 2) :|north| (9 10 11))
         :|texture| "tf_lion_cub" :|name| "lion_cub")
        :|lioness|
        (:|anims|
         (:|west| (3 4 5) :|east| (6 7 8) :|south| (0 1 2) :|north| (9 10 11))
         :|texture| "tf_lioness" :|name| "lioness")
        :|lion|
        (:|anims|
         (:|west| (3 4 5) :|east| (6 7 8) :|south| (0 1 2) :|north| (9 10 11))
         :|texture| "tf_lion" :|name| "lion")
        :|kangaroo|
        (:|anims|
         (:|west| (3 4 5) :|east| (6 7 8) :|south| (0 1 2) :|north| (9 10 11))
         :|texture| "tf_kangaroo" :|name| "kangaroo")
        ;; :|jungle_tileset|
        ;; (:|anims|
        ;;  (:|west| (3 4 5) :|east| (6 7 8) :|south| (0 1 2) :|north| (9 10 11))
        ;;  :|texture| "tf_jungle_tileset" :|name| "jungle_tileset")
        :|horseshoe_crab|
        (:|anims|
         (:|west| (3 4 5) :|east| (6 7 8) :|south| (0 1 2) :|north| (9 10 11))
         :|texture| "tf_horseshoe_crab" :|name| "horseshoe_crab")
        ;; :|hippo_water|
        ;; (:|anims|
        ;;  (:|west| (3 4 5) :|east| (6 7 8) :|south| (0 1 2) :|north| (9 10 11))
        ;;  :|texture| "tf_hippo_water" :|name| "hippo_water")
        :|hippo|
        (:|anims|
         (:|west| (3 4 5) :|east| (6 7 8) :|south| (0 1 2) :|north| (9 10 11))
         :|texture| "tf_hippo" :|name| "hippo")
        :|gorilla|
        (:|anims|
         (:|west| (3 4 5) :|east| (6 7 8) :|south| (0 1 2) :|north| (9 10 11))
         :|texture| "tf_gorilla" :|name| "gorilla")
        :|frog|
        (:|anims|
         (:|west| (3 4 5) :|east| (6 7 8) :|south| (0 1 2) :|north| (9 10 11))
         :|texture| "tf_frog" :|name| "frog")
        ;; :|fish_8sheet|
        ;; (:|anims|
        ;;  (:|west| (3 4 5) :|east| (6 7 8) :|south| (0 1 2) :|north| (9 10 11))
        ;;  :|texture| "tf_fish_8sheet" :|name| "fish_8sheet")
        :|elephant_baby|
        (:|anims|
         (:|west| (3 4 5) :|east| (6 7 8) :|south| (0 1 2) :|north| (9 10 11))
         :|texture| "tf_elephant_baby" :|name| "elephant_baby")
        :|elephant|
        (:|anims|
         (:|west| (3 4 5) :|east| (6 7 8) :|south| (0 1 2) :|north| (9 10 11))
         :|texture| "tf_elephant" :|name| "elephant")
        ;; :|crocodile_water|
        ;; (:|anims|
        ;;  (:|west| (3 4 5) :|east| (6 7 8) :|south| (0 1 2) :|north| (9 10 11))
        ;;  :|texture| "tf_crocodile_water" :|name| "crocodile_water")
        ;; :|crocodile|
        ;; (:|anims|
        ;;  (:|west| (3 4 5) :|east| (6 7 8) :|south| (0 1 2) :|north| (9 10 11))
        ;;  :|texture| "tf_crocodile" :|name| "crocodile")
        ;; :|crab|
        ;; (:|anims|
        ;;  (:|west| (3 4 5) :|east| (6 7 8) :|south| (0 1 2) :|north| (9 10 11))
        ;;  :|texture| "tf_crab" :|name| "crab")
        :|camel_b_pack|
        (:|anims|
         (:|west| (3 4 5) :|east| (6 7 8) :|south| (0 1 2) :|north| (9 10 11))
         :|texture| "tf_camel_b_pack" :|name| "camel_b_pack")
        :|camel_b|
        (:|anims|
         (:|west| (3 4 5) :|east| (6 7 8) :|south| (0 1 2) :|north| (9 10 11))
         :|texture| "tf_camel_b" :|name| "camel_b")
        :|camel_a_pack|
        (:|anims|
         (:|west| (3 4 5) :|east| (6 7 8) :|south| (0 1 2) :|north| (9 10 11))
         :|texture| "tf_camel_a_pack" :|name| "camel_a_pack")
        :|camel_a|
        (:|anims|
         (:|west| (3 4 5) :|east| (6 7 8) :|south| (0 1 2) :|north| (9 10 11))
         :|texture| "tf_camel_a" :|name| "camel_a")
        :|bugs_8sheet|
        (:|anims|
         (:|west| (3 4 5) :|east| (6 7 8) :|south| (0 1 2) :|north| (9 10 11))
         :|texture| "tf_bugs_8sheet" :|name| "bugs_8sheet")
        :|buffalo|
        (:|anims|
         (:|west| (3 4 5) :|east| (6 7 8) :|south| (0 1 2) :|north| (9 10 11))
         :|texture| "tf_buffalo" :|name| "buffalo")
        :|bison|
        (:|anims|
         (:|west| (3 4 5) :|east| (6 7 8) :|south| (0 1 2) :|north| (9 10 11))
         :|texture| "tf_bison" :|name| "bison")
        :|birds_8sheet|
        (:|anims|
         (:|west| (3 4 5) :|east| (6 7 8) :|south| (0 1 2) :|north| (9 10 11))
         :|texture| "tf_birds_8sheet" :|name| "birds_8sheet")
        :|bee|
        (:|anims|
         (:|west| (3 4 5) :|east| (6 7 8) :|south| (0 1 2) :|north| (9 10 11))
         :|texture| "tf_bee" :|name| "bee")
        :|blacklamb|
        (:|anims|
         (:|west| (69 70 71) :|east| (81 82 83) :|south| (57 58 59) :|north|
          (93 94 95))
         :|texture| "animals4" :|name| "blacklamb")
        :|blacksheep|
        (:|anims|
         (:|west| (21 22 23) :|east| (33 34 35) :|south| (9 10 11) :|north|
          (45 46 47))
         :|texture| "animals4" :|name| "blacksheep")
        :|lamb2|
        (:|anims|
         (:|west| (66 67 68) :|east| (78 79 80) :|south| (54 55 56) :|north|
          (90 91 92))
         :|texture| "animals4" :|name| "lamb2")
        :|sheep2|
        (:|anims|
         (:|west| (18 19 20) :|east| (30 31 32) :|south| (6 7 8) :|north| (42 43 44))
         :|texture| "animals4" :|name| "sheep2")
        :|lamb|
        (:|anims|
         (:|west| (60 61 62) :|east| (72 73 74) :|south| (48 49 50) :|north|
          (84 85 86))
         :|texture| "animals4" :|name| "lamb")
        :|sheep|
        (:|anims|
         (:|west| (12 13 14) :|east| (24 25 26) :|south| (0 1 2) :|north| (36 37 38))
         :|texture| "animals4" :|name| "sheep")
        :|kittendogblack|
        (:|anims|
         (:|west| (63 64 65) :|east| (75 76 77) :|south| (51 52 53) :|north|
          (87 88 89))
         :|texture| "animals1" :|name| "kittendogblack")
        :|kittendog|
        (:|anims|
         (:|west| (60 61 62) :|east| (72 73 74) :|south| (48 49 50) :|north|
          (84 85 86))
         :|texture| "animals1" :|name| "kittendog")
        :|puppy-brown|
        (:|anims|
         (:|west| (18 19 20) :|east| (30 31 32) :|south| (6 7 8) :|north| (42 43 44))
         :|texture| "animals1" :|name| "puppy-brown")
        :|puppy-golden|
        (:|anims|
         (:|west| (15 16 17) :|east| (27 28 29) :|south| (3 4 5) :|north| (39 40 41))
         :|texture| "animals1" :|name| "puppy-golden")
        :|puppy-gray|
        (:|anims|
         (:|west| (12 13 14) :|east| (24 25 26) :|south| (0 1 2) :|north| (36 37 38))
         :|texture| "animals1" :|name| "puppy-gray")
        :|baby-chipmunk|
        (:|anims|
         (:|west| (15 16 17) :|east| (27 28 29) :|south| (3 4 5) :|north| (39 40 41))
         :|texture| "animals2" :|name| "baby-chipmunk")
        :|red-eyed-bunny|
        (:|anims|
         (:|west| (21 22 23) :|east| (33 34 35) :|south| (9 10 11) :|north|
          (45 46 47))
         :|texture| "animals2" :|name| "red-eyed-bunny")
        :|fox|
        (:|anims|
         (:|west| (69 70 71) :|east| (81 82 83) :|south| (57 58 59) :|north|
          (93 94 95))
         :|texture| "animals2" :|name| "fox")
        :|littlebull|
        (:|anims|
         (:|west| (21 22 23) :|east| (33 34 35) :|south| (9 10 11) :|north|
          (45 46 47))
         :|texture| "animals3" :|name| "littlebull")
        :|bullygoat|
        (:|anims|
         (:|west| (69 70 71) :|east| (81 82 83) :|south| (57 58 59) :|north|
          (93 94 95))
         :|texture| "animals3" :|name| "bullygoat")
        :|little-goat|
        (:|anims|
         (:|west| (66 67 68) :|east| (78 79 80) :|south| (54 55 56) :|north|
          (90 91 92))
         :|texture| "animals3" :|name| "little-goat")
        :|littlepig|
        (:|anims|
         (:|west| (63 64 65) :|east| (75 76 77) :|south| (51 52 53) :|north|
          (87 88 89))
         :|texture| "animals3" :|name| "littlepig")
        :|pig|
        (:|anims|
         (:|west| (60 61 62) :|east| (72 73 74) :|south| (48 49 50) :|north|
          (84 85 86))
         :|texture| "animals3" :|name| "pig")
        :|bull|
        (:|anims|
         (:|west| (18 19 20) :|east| (30 31 32) :|south| (6 7 8) :|north| (42 43 44))
         :|texture| "animals3" :|name| "bull")
        :|littlecow|
        (:|anims|
         (:|west| (15 16 17) :|east| (27 28 29) :|south| (3 4 5) :|north| (39 40 41))
         :|texture| "animals3" :|name| "littlecow")
        :|bigcow|
        (:|anims|
         (:|west| (12 13 14) :|east| (24 25 26) :|south| (0 1 2) :|north| (36 37 38))
         :|texture| "animals3" :|name| "bigcow")
        :|player|
        (:|charGroups| (nil nil) :|body|
         (:|height| nil :|width| nil :|offsetY| 12 :|offsetX| 0) :|scale| 1
         :|dashDistance| nil :|speed| 62 :|anims|
         (:|west| (12 13 14) :|east| (24 25 26) :|south| (0 1 2) :|north| (36 37 38))
         :|texture| "animals2" :|name| "player")
        ;; :|playerturtle|
        ;; (:|charGroups| (nil nil) :|body|
        ;;  (:|height| nil :|width| nil :|offsetY| nil :|offsetX| nil) :|scale| 1
        ;;  :|dashDistance| nil :|speed| 22 :|anims|
        ;;  (:|west| (12 13 14) :|east| (24 25 26) :|south| (0 1 2) :|north| (36 37 38))
        ;;  :|texture| "turtles" :|name| "playerturtle")
        ;; :|Moby|
        ;; (:|charGroups| (nil nil) :|body|
        ;;  (:|height| 14 :|width| 12 :|offsetY| nil :|offsetX| nil) :|scale| nil
        ;;  :|dashDistance| nil :|speed| 24 :|anims|
        ;;  (:|west| (60 61 62) :|east| (72 73 74) :|south| (48 49 50) :|north|
        ;;   (84 85 86))
        ;;  :|texture| "animals3" :|name| "Moby")
        ;; :|Socrates|
        ;; (:|charGroups| (nil nil) :|body|
        ;;  (:|height| 14 :|width| 12 :|offsetY| nil :|offsetX| nil) :|scale| nil
        ;;  :|dashDistance| nil :|speed| 24 :|anims|
        ;;  (:|west| (60 61 62) :|east| (72 73 74) :|south| (48 49 50) :|north|
        ;;   (84 85 86))
        ;;  :|texture| "animals3" :|name| "Socrates")
        ;; :|Phaedrus|
        ;; (:|charGroups| (nil nil) :|body|
        ;;  (:|height| nil :|width| nil :|offsetY| nil :|offsetX| nil) :|scale| nil
        ;;  :|dashDistance| nil :|speed| 32 :|anims|
        ;;  (:|west| (21 22 23) :|east| (33 34 35) :|south| (9 10 11) :|north|
        ;;   (45 46 47))
        ;;  :|texture| "animals2" :|name| "Phaedrus")
        ;; :|Joe|
        ;; (:|charGroups| (nil nil) :|body|
        ;;  (:|height| 14 :|width| 12 :|offsetY| nil :|offsetX| nil) :|scale| nil
        ;;  :|dashDistance| nil :|speed| 24 :|anims|
        ;;  (:|west| (12 13 14) :|east| (24 25 26) :|south| (0 1 2) :|north| (36 37 38))
        ;;  :|texture| "turtles" :|name| "Joe")
        ;; :|Daniel|
        ;; (:|charGroups| (nil nil) :|body|
        ;;  (:|height| 14 :|width| 12 :|offsetY| nil :|offsetX| nil) :|scale| nil
        ;;  :|dashDistance| nil :|speed| 24 :|anims|
        ;;  (:|west| (60 61 62) :|east| (72 73 74) :|south| (48 49 50) :|north|
        ;;   (84 85 86))
        ;;  :|texture| "animals3" :|name| "Daniel")
        ;; :|Carl|
        ;; (:|charGroups| (nil nil) :|body|
        ;;  (:|height| 14 :|width| 12 :|offsetY| nil :|offsetX| nil) :|scale| nil
        ;;  :|dashDistance| nil :|speed| 24 :|anims|
        ;;  (:|west| (3 4 5) :|east| (6 7 8) :|south| (0 1 2) :|north| (9 10 11))
        ;;  :|texture| "male102" :|name| "Carl")
        ;; :|Brandon|
        ;; (:|charGroups| (nil nil) :|body|
        ;;  (:|height| 14 :|width| 12 :|offsetY| nil :|offsetX| nil) :|scale| nil
        ;;  :|dashDistance| nil :|speed| 32 :|anims|
        ;;  (:|west| (3 4 5) :|east| (6 7 8) :|south| (0 1 2) :|north| (9 10 11))
        ;;  :|texture| "studentmale" :|name| "Brandon")
        ;; :|Maik|
        ;; (:|charGroups| (nil nil) :|body|
        ;;  (:|height| nil :|width| nil :|offsetY| nil :|offsetX| nil) :|scale| 0.5
        ;;  :|dashDistance| nil :|speed| 32 :|anims|
        ;;  (:|west| (3 4 5) :|east| (6 7 8) :|south| (0 1 2) :|north| (9 10 11))
        ;;  :|texture| "studentmale" :|name| "Maik")
        ;; :|Golab|
        ;; (:|charGroups| (nil nil) :|body|
        ;;  (:|height| 14 :|width| 12 :|offsetY| nil :|offsetX| nil) :|scale| nil
        ;;  :|dashDistance| nil :|speed| 24 :|anims|
        ;;  (:|west| (3 4 5) :|east| (6 7 8) :|south| (0 1 2) :|north| (9 10 11))
        ;;  :|texture| "ghost1" :|name| "Golab")
        ;; :|default|
        ;; (:|charGroups| (nil nil) :|body|
        ;;  (:|height| nil :|width| nil :|offsetY| nil :|offsetX| nil) :|scale| nil
        ;;  :|dashDistance| nil :|speed| 52 :|anims|
        ;;  (:|west| (12 13 14) :|east| (24 25 26) :|south| (0 1 2) :|north| (36 37 38))
        ;;  :|texture| "workers" :|name| "default")
        ;; :|tweeter1|
        ;; (:|charGroups| ("tweeters" nil) :|body|
        ;;  (:|height| nil :|width| nil :|offsetY| nil :|offsetX| nil) :|scale| nil
        ;;  :|dashDistance| nil :|speed| 52 :|anims|
        ;;  (:|west| (12 13 14) :|east| (24 25 26) :|south| (0 1 2) :|north| (36 37 38))
        ;;  :|texture| "workers" :|name| "tweeter1")
        ;; :|tweeter2|
        ;; (:|charGroups| ("tweeters" nil) :|body|
        ;;  (:|height| nil :|width| nil :|offsetY| nil :|offsetX| nil) :|scale| nil
        ;;  :|dashDistance| nil :|speed| 52 :|anims|
        ;;  (:|west| (15 16 17) :|east| (27 28 29) :|south| (3 4 5) :|north| (39 40 41))
        ;;  :|texture| "workers" :|name| "tweeter2")
        ;; :|tweeter3|
        ;; (:|charGroups| ("tweeters" nil) :|body|
        ;;  (:|height| nil :|width| nil :|offsetY| nil :|offsetX| nil) :|scale| nil
        ;;  :|dashDistance| nil :|speed| 52 :|anims|
        ;;  (:|west| (18 19 20) :|east| (30 31 32) :|south| (6 7 8) :|north| (42 43 44))
        ;;  :|texture| "workers" :|name| "tweeter3")
        ;; :|tweeter4|
        ;; (:|charGroups| ("tweeters" nil) :|body|
        ;;  (:|height| nil :|width| nil :|offsetY| nil :|offsetX| nil) :|scale| nil
        ;;  :|dashDistance| nil :|speed| 52 :|anims|
        ;;  (:|west| (21 22 23) :|east| (33 34 35) :|south| (9 10 11) :|north|
        ;;   (45 46 47))
        ;;  :|texture| "workers" :|name| "tweeter4")
        ;; :|tweeter5|
        ;; (:|charGroups| ("tweeters" nil) :|body|
        ;;  (:|height| nil :|width| nil :|offsetY| nil :|offsetX| nil) :|scale| nil
        ;;  :|dashDistance| nil :|speed| 52 :|anims|
        ;;  (:|west| (60 61 62) :|east| (72 73 74) :|south| (48 49 50) :|north|
        ;;   (84 85 86))
        ;;  :|texture| "workers" :|name| "tweeter5")
        ;; :|tweeter6|
        ;; (:|charGroups| ("tweeters" nil) :|body|
        ;;  (:|height| nil :|width| nil :|offsetY| nil :|offsetX| nil) :|scale| nil
        ;;  :|dashDistance| nil :|speed| 52 :|anims|
        ;;  (:|west| (63 64 65) :|east| (75 76 77) :|south| (51 52 53) :|north|
        ;;   (87 88 89))
        ;;  :|texture| "workers" :|name| "tweeter6")
        ;; :|tweeter7|
        ;; (:|charGroups| ("tweeters" nil) :|body|
        ;;  (:|height| nil :|width| nil :|offsetY| nil :|offsetX| nil) :|scale| nil
        ;;  :|dashDistance| nil :|speed| 52 :|anims|
        ;;  (:|west| (66 67 68) :|east| (78 79 80) :|south| (54 55 56) :|north|
        ;;   (90 91 92))
        ;;  :|texture| "workers" :|name| "tweeter7")
        ;; :|tweeter8|
        ;; (:|charGroups| ("tweeters" nil) :|body|
        ;;  (:|height| nil :|width| nil :|offsetY| nil :|offsetX| nil) :|scale| nil
        ;;  :|dashDistance| nil :|speed| 52 :|anims|
        ;;  (:|west| (69 70 71) :|east| (81 82 83) :|south| (57 58 59) :|north|
        ;;   (93 94 95))
        ;;  :|texture| "workers" :|name| "tweeter8")
        ;; :|lilMoby|
        ;; (:|charGroups| (nil nil) :|body|
        ;;  (:|height| nil :|width| nil :|offsetY| nil :|offsetX| nil) :|scale| nil
        ;;  :|dashDistance| nil :|speed| 52 :|anims|
        ;;  (:|west| (63 64 65) :|east| (75 76 77) :|south| (51 52 53) :|north|
        ;;   (87 88 89))
        ;;  :|texture| "animals3" :|name| "lilMoby")
        ;; :|Manda|
        ;; (:|charGroups| (nil nil) :|body|
        ;;  (:|height| nil :|width| nil :|offsetY| nil :|offsetX| nil) :|scale| nil
        ;;  :|dashDistance| nil :|speed| 20 :|anims|
        ;;  (:|west| (60 61 62) :|east| (72 73 74) :|south| (48 49 50) :|north|
        ;;   (84 85 86))
        ;;  :|texture| "animals3" :|name| "Manda")
        :|gopher|
        (:|charGroups| (nil nil) :|body|
         (:|height| nil :|width| nil :|offsetY| nil :|offsetX| nil) :|scale| nil
         :|dashDistance| nil :|speed| 20 :|anims|
         (:|west| (15 16 17) :|east| (27 28 29) :|south| (3 4 5) :|north| (39 40 41))
         :|texture| "animals2" :|name| "gopher")
        :|bunny|
        (:|charGroups| (nil nil) :|body|
         (:|height| nil :|width| nil :|offsetY| 130 :|offsetX| nil) :|scale| nil
         :|dashDistance| nil :|speed| 32 :|anims|
         (:|west| (18 19 20) :|east| (30 31 32) :|south| (6 7 8) :|north| (42 43 44))
         :|texture| "animals2" :|name| "bunny")
        :|graybunny|
        (:|charGroups| (nil nil) :|body|
         (:|height| nil :|width| nil :|offsetY| nil :|offsetX| nil) :|scale| nil
         :|dashDistance| nil :|speed| 32 :|anims|
         (:|west| (21 22 23) :|east| (33 34 35) :|south| (9 10 11) :|north|
          (45 46 47))
         :|texture| "animals2" :|name| "graybunny")
        :|grayraccoon|
        (:|charGroups| (nil nil) :|body|
         (:|height| nil :|width| nil :|offsetY| nil :|offsetX| nil) :|scale| nil
         :|dashDistance| nil :|speed| 32 :|anims|
         (:|west| (63 64 65) :|east| (75 76 77) :|south| (51 52 53) :|north|
          (87 88 89))
         :|texture| "animals2" :|name| "grayraccoon")
        :|raccoon|
        (:|charGroups| (nil nil) :|body|
         (:|height| nil :|width| nil :|offsetY| nil :|offsetX| nil) :|scale| nil
         :|dashDistance| nil :|speed| 32 :|anims|
         (:|west| (66 67 68) :|east| (78 79 80) :|south| (54 55 56) :|north|
          (90 91 92))
         :|texture| "animals2" :|name| "raccoon")))

(setq platforms
      '(:|default|
        (:|edgeTiles| (5) :|groundTiles| (15 16) :|texture| "scut_extrude-16" :|name|
         "default")
        :|cobblestone|
        (:|edgeTiles| (475) :|groundTiles| (414 415 416 434 435 436) :|texture|
         "browserquestextrude" :|name| "cobblestone")))

(setf *world-data*
      (list :|mapobjects| mapobjects
            :|sounds| sounds
            :|characters| characters
            :|images| images
            :|platforms| platforms))



(defun get-random-character ()
  (alexandria:random-elt
   (loop for (key value) on characters
           by #'cddr collect (getf value :|name|))))


;; (alexandria:write-string-into-file
;;  (jojo:to-json
;;   *world-data*)
;;  "/home/mik/joegame/packages/mapexplorer/lib/jdb.json"
;;  :if-exists :supersede)
