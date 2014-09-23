SELECT
  s.student_number,
  s.last_name,
  s.first_name,
  sch.abbreviation school,
  s.grade_level true_grade,
	CASE
	WHEN se.benchmark_grade_level_ela IS NOT NULL
	THEN CAST(se.benchmark_grade_level_ela AS VARCHAR2(10))
	ELSE CAST(s.grade_level AS VARCHAR2(10))
	END ela_benchmark_grade,
  s.entrydate entry_date,
  s.home_room,
  se.laa1,
  se.rti_not_star
FROM students s
LEFT JOIN u_def_ext_students se ON se.studentsdcid = s.dcid
JOIN schools sch ON s.schoolid = sch.school_number
WHERE s.grade_level > -1
AND sch.school_number IN (1,2,3,6,369701)
AND s.enroll_status = 0