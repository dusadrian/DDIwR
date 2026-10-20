# SPSS menu validation

Generate files with `Rscript tools/readstat/spss-interop.R NEW_DIRECTORY`.
Use only the generated synthetic files. Keep the user's existing named dataset
open throughout. The menu check is partial until native saved copies have been
returned and compared with the originals in R.

1. Choose File > Open > Data and open `none.sav`, then `byte.sav`,
   `zsav.zsav` and `empty.sav` separately. Each opens as a new dataset.
2. In Data View, check three rows (zero for empty), five columns, IDs 1/2/3,
   first-choice values 1/0/1, and strings `Yes é`, `No`, `漢字`.
3. In Variable View, check `first`: numeric width 9, decimals 2, label
   First choice, No=0/Yes=1, columns 17, center alignment, ordinal measure.
   Check `text_one`: string width 20, columns 23, left alignment, nominal.
4. When licensed, use Data > Define Multiple Response Sets to inspect
   `$categories`, `$picked`, `$counted`, `$words`. Check categories versus
   dichotomies, variables, counted values and label sources against the bundle
   generator. Do not redefine the sets while inspecting them.
5. When licensed, choose File > Save As for each synthetic dataset and save to
   a fresh filename. Return only those synthetic files for comparison of values,
   vendor metadata, MR sets, dataset/variable attributes, notes and labels in R.

The available VM trial allowed opening and viewing `none.sav`; Save As and
Define Multiple Response Sets are locked. Save As showed Add-On Required.
This proves successful import of that file and visible values/metadata only.
It does not prove native save-back, all compression modes, custom attributes,
notes or MR-set semantics. Syntax/Python assertions in `validate.sps` remain
prepared but unexecuted under that licence.

IBM documents its free trial as including all subscription features. The observed
locks therefore suggest an activation/entitlement problem in this session,
rather than an established restriction of all SPSS trials. No licence settings
were changed, and SPSS was not closed. IBM's current trial description:
https://www.ibm.com/products/spss-statistics/resources
