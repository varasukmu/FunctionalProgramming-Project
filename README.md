# FunctionalProgramming-Project

วิธีการใช้งาน (Run Commands)

| คำสั่ง | โหมดการทำงาน | คำอธิบาย |
|--------|----------------|-----------|
| "sbt ""run sequential""" | Sequential | ประมวลผลบน Thread เดียว (มาตรฐาน) |
| "sbt ""run parallel""" | Parallel | กระจายงานลงทุก CPU Core เพื่อความเร็ว |
| "sbt ""run benchmark""" | Benchmark | รันทั้ง 2 ระบบเพื่อเปรียบเทียบเวลาประมวลผล |