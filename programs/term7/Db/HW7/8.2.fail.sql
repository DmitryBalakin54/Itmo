TRUNCATE Marks, Plan, Students, Groups, Lecturers, Courses RESTART IDENTITY CASCADE;

INSERT INTO Groups (GroupId, GroupName) VALUES
    (1, 'G1'),
    (2, 'G2');

INSERT INTO Students (StudentId, StudentName, GroupId) VALUES
    (1, 'Иванов', 1),
    (2, 'Петров', 1),
    (3, 'Сидоров', 2);

INSERT INTO Lecturers (LecturerId, LecturerName) VALUES
    (1, 'Лектор А'),
    (2, 'Лектор Б');

INSERT INTO Courses (CourseId, CourseName) VALUES
    (101, 'Математика'),
    (102, 'Физика'),
    (103, 'Информатика'),
    (104, 'История');

INSERT INTO Plan (GroupId, CourseId, LecturerId) VALUES
    (1, 101, 1),
    (1, 102, 2),
    (2, 103, 1),
    (2, 104, 2);

INSERT INTO Marks (StudentId, CourseId, Mark) VALUES
    (1, 101, 90),
    (1, 102, 80),
    (2, 101, 85),
    (2, 102, 75),
    (3, 103, 95);

BEGIN;

-- Попытка вставки курса, которого нет у всех студентов группы
SAVEPOINT sp1;
INSERT INTO Marks (StudentId, CourseId, Mark) VALUES (2, 103, 88);
EXCEPTION WHEN OTHERS THEN
    ROLLBACK TO SAVEPOINT sp1;

-- Попытка вставки курса для другого студента
SAVEPOINT sp2;
INSERT INTO Marks (StudentId, CourseId, Mark) VALUES (1, 103, 92);
EXCEPTION WHEN OTHERS THEN
    ROLLBACK TO SAVEPOINT sp2;

-- Попытка обновления курса, чтобы нарушить одинаковый набор курсов
SAVEPOINT sp3;
UPDATE Marks SET CourseId = 104 WHERE StudentId = 1 AND CourseId = 102;
EXCEPTION WHEN OTHERS THEN
    ROLLBACK TO SAVEPOINT sp3;

-- Попытка перевести студента в другую группу
SAVEPOINT sp4;
UPDATE Students SET GroupId = 2 WHERE StudentId = 1;
EXCEPTION WHEN OTHERS THEN
    ROLLBACK TO SAVEPOINT sp4;

-- Попытка удалить оценку, нарушающую правило одинакового набора
SAVEPOINT sp5;
DELETE FROM Marks WHERE StudentId = 2 AND CourseId = 102;
EXCEPTION WHEN OTHERS THEN
    ROLLBACK TO SAVEPOINT sp5;

COMMIT;

SELECT * FROM Marks ORDER BY StudentId, CourseId;
