-- PostgreSQL 17.4

CREATE OR REPLACE FUNCTION check_marks()
RETURNS TRIGGER AS $$
BEGIN
    IF NOT EXISTS (
        SELECT 1
        FROM Students AS S
        INNER JOIN Plan AS P ON S.GroupId = P.GroupId
        WHERE S.StudentId = NEW.StudentId
        AND P.CourseId = NEW.CourseId
    ) THEN
        RAISE EXCEPTION 'NoExtraMarks: student % does not have course % in group plan', NEW.StudentId, NEW.CourseId;
    END IF;

    RETURN NEW;
END;
$$ LANGUAGE plpgsql;

DROP TRIGGER IF EXISTS NoExtraMarks ON Marks;
CREATE TRIGGER NoExtraMarks
BEFORE INSERT OR UPDATE ON Marks
FOR EACH ROW
EXECUTE FUNCTION check_marks();


CREATE OR REPLACE FUNCTION check_students()
RETURNS TRIGGER AS $$
BEGIN
    IF OLD.StudentId IS DISTINCT FROM NEW.StudentId THEN
        IF EXISTS (
            SELECT 1
            FROM Marks AS M
            WHERE M.StudentId = OLD.StudentId
        ) THEN
            RAISE EXCEPTION 'NoExtraMarksForStudents: cannot change StudentId % -> %, marks exist for this student.', OLD.StudentId, NEW.StudentId;
        END IF;
    END IF;

    IF OLD.GroupId IS DISTINCT FROM NEW.GroupId THEN
        IF EXISTS (
            SELECT 1
            FROM Marks AS M
            LEFT JOIN Plan AS P ON P.GroupId = NEW.GroupId AND P.CourseId = M.CourseId
            WHERE M.StudentId = NEW.StudentId
            AND P.CourseId IS NULL
        ) THEN
            RAISE EXCEPTION 'NoExtraMarksForStudents: student % has marks for courses that are not in new group % plan', NEW.StudentId, NEW.GroupId;
        END IF;
    END IF;

    RETURN NEW;
END;
$$ LANGUAGE plpgsql;

DROP TRIGGER IF EXISTS NoExtraMarksForStudents ON Students;
CREATE TRIGGER NoExtraMarksForStudents
BEFORE UPDATE ON Students
FOR EACH ROW
EXECUTE FUNCTION check_students();


CREATE OR REPLACE FUNCTION check_plan()
RETURNS TRIGGER AS $$
BEGIN
    IF TG_OP = 'DELETE'
       OR OLD.GroupId IS DISTINCT FROM NEW.GroupId
       OR OLD.CourseId IS DISTINCT FROM NEW.CourseId THEN
        IF EXISTS (
            SELECT 1
            FROM Students AS S
            JOIN Marks AS M ON S.StudentId = M.StudentId
            WHERE S.GroupId = OLD.GroupId
            AND M.CourseId = OLD.CourseId
        ) THEN
            RAISE EXCEPTION 'NoExtraMarksForPlan: cannot remove or move plan entry (GroupId %, CourseId %) because marks exist.', OLD.GroupId, OLD.CourseId;
        END IF;
    END IF;
    RETURN NEW;
END;
$$ LANGUAGE plpgsql;

DROP TRIGGER IF EXISTS NoExtraMarksForPlan ON Plan;
CREATE TRIGGER NoExtraMarksForPlan
BEFORE DELETE OR UPDATE ON Plan
FOR EACH ROW
EXECUTE FUNCTION check_plan();
