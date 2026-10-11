These three binary regression fixtures are copied unchanged from
https://github.com/WizardMac/ReadStat/tree/b05b645661779ce1863d9fa35a84a7542027085a/resources
under ReadStat's MIT license (retained at `src/readstat/LICENSE`).

- `strl_119.dta`: format 119 references resolve to `third`, `fourth`.
- `trailing_blanks_118.dta`: significant blanks survive as `a     `, `b  `, and an empty string.
- `binary_strl_118.dta`: binary string records do not disrupt subsequent references (`bin`, `hello`).

Expected values come from upstream's `src/test/test_list.h` at the same revision.
The additional generated 20,001-row tests exercise DDIwR's actual parallel workers.
