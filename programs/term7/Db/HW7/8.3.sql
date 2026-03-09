-- PostgreSQL 17.4

CREATE OR REPLACE FUNCTION preserve_marks()
RETURNS TRIGGER AS $$
BEGIN
    IF NEW.Mark < OLD.Mark THEN
        RAISE EXCEPTION 'PreserveMarks: cannot decrease mark for student % in course % (current: %, attempted: %)',
            NEW.StudentId, NEW.CourseId, OLD.Mark, NEW.Mark;
    END IF;

    RETURN NEW;
END;
$$ LANGUAGE plpgsql;

DROP TRIGGER IF EXISTS PreserveMarks ON Marks;
CREATE TRIGGER PreserveMarks
BEFORE UPDATE ON Marks
FOR EACH ROW
EXECUTE FUNCTION preserve_marks();

