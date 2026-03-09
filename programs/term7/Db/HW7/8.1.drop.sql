DROP TRIGGER IF EXISTS NoExtraMarks ON Marks;
DROP FUNCTION IF EXISTS check_marks();

DROP TRIGGER IF EXISTS NoExtraMarksForStudents ON Students;
DROP FUNCTION IF EXISTS check_students();

DROP TRIGGER IF EXISTS NoExtraMarksForPlan ON Plan;
DROP FUNCTION IF EXISTS check_plan();

