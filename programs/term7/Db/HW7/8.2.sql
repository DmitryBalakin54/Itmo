-- PostgreSQL 17.4

CREATE OR REPLACE FUNCTION check_same_marks()
RETURNS TRIGGER AS $$
DECLARE
    groupIdTmp INT;
    hasDiff BOOLEAN;
    studentIdTmp INT;
BEGIN
    IF TG_OP = 'DELETE' THEN
        studentIdTmp := OLD.StudentId;
    ELSE
        studentIdTmp := NEW.StudentId;
    END IF;

    SELECT GroupId INTO groupIdTmp
    FROM Students
    WHERE StudentId = studentIdTmp;

    SELECT EXISTS (
        SELECT 1
        FROM Students AS S1
        JOIN Students AS S2 ON S1.GroupId = S2.GroupId AND S1.StudentId < S2.StudentId
        WHERE S1.GroupId = groupIdTmp
        AND (
            EXISTS (
                SELECT CourseId
                FROM Marks
                WHERE StudentId = S1.StudentId

                EXCEPT

                SELECT CourseId
                FROM Marks
                WHERE StudentId = S2.StudentId
            )
            OR EXISTS (
                SELECT CourseId
                FROM Marks
                WHERE StudentId = S2.StudentId

                EXCEPT

                SELECT CourseId
                FROM Marks
                WHERE StudentId = S1.StudentId
            )
        )
    ) INTO hasDiff;

    IF hasDiff THEN
        RAISE EXCEPTION 'SameMarks: not all students in group % have the same set of courses', groupIdTmp;
    END IF;

    IF TG_OP = 'DELETE' THEN
        RETURN OLD;
    ELSE
        RETURN NEW;
    END IF;
END;
$$ LANGUAGE plpgsql;

DROP TRIGGER IF EXISTS SameMarks ON Marks;
CREATE TRIGGER SameMarks
AFTER INSERT OR UPDATE OR DELETE ON Marks
FOR EACH ROW
EXECUTE FUNCTION check_same_marks();
