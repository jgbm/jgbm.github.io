; ModuleID = 'lab5.c'
target datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

@.str = private unnamed_addr constant [16 x i8] c"%lld %lld %lld\0A\00", align 1

; Function Attrs: nounwind uwtable
define i64 @f(i64 %a) #0 {
  %1 = alloca i64, align 8
  %i = alloca i64, align 8
  store i64 %a, i64* %1, align 8
  store i64 0, i64* %i, align 8
  br label %2

; <label>:2                                       ; preds = %17, %0
  %3 = load i64, i64* %1, align 8
  %4 = icmp sgt i64 %3, 1
  br i1 %4, label %5, label %20

; <label>:5                                       ; preds = %2
  %6 = load i64, i64* %1, align 8
  %7 = and i64 %6, 1
  %8 = icmp eq i64 %7, 0
  br i1 %8, label %9, label %12

; <label>:9                                       ; preds = %5
  %10 = load i64, i64* %1, align 8
  %11 = ashr i64 %10, 1
  store i64 %11, i64* %1, align 8
  br label %16

; <label>:12                                      ; preds = %5
  %13 = load i64, i64* %1, align 8
  %14 = mul nsw i64 3, %13
  %15 = add nsw i64 %14, 1
  store i64 %15, i64* %1, align 8
  br label %16

; <label>:16                                      ; preds = %12, %9
  br label %17

; <label>:17                                      ; preds = %16
  %18 = load i64, i64* %i, align 8
  %19 = add nsw i64 %18, 1
  store i64 %19, i64* %i, align 8
  br label %2

; <label>:20                                      ; preds = %2
  %21 = load i64, i64* %i, align 8
  ret i64 %21
}

; Function Attrs: nounwind uwtable
define i32 @main() #0 {
  %1 = alloca i32, align 4
  store i32 0, i32* %1, align 4
  %2 = call i64 @f(i64 5)
  %3 = call i64 @f(i64 9)
  %4 = call i64 @f(i64 64)
  %5 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([16 x i8], [16 x i8]* @.str, i32 0, i32 0), i64 %2, i64 %3, i64 %4)
  ret i32 0
}

declare i32 @printf(i8*, ...) #1

attributes #0 = { nounwind uwtable "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+fxsr,+mmx,+sse,+sse2" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+fxsr,+mmx,+sse,+sse2" "unsafe-fp-math"="false" "use-soft-float"="false" }

!llvm.ident = !{!0}

!0 = !{!"clang version 3.8.0-2ubuntu4 (tags/RELEASE_380/final)"}
