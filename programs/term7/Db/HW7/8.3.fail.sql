-- Очистка таблиц
TRUNCATE Marks, Plan, Students, Groups, Lecturers, Courses RESTART IDENTITY CASCADE;

-- Создание данных
INSERT INTO Groups (GroupId, GroupName) VALUES (1, 'G1');
INSERT INTO Students (StudentId, StudentName, GroupId) VALUES (1, 'Иванов', 1), (2, 'Петров', 1);
INSERT INTO Courses (CourseId, CourseName) VALUES (101, 'Математика'), (102, 'Физика');
INSERT INTO Marks (StudentId, CourseId, Mark) VALUES
(1, 101, 85),
(1, 102, 90),
(2, 101, 75),
(2, 102, 80);

BEGIN;

-- Попытки уменьшить оценки с отловом ошибок через SAVEPOINT
SAVEPOINT sp1;
UPDATE Marks SET Mark = 80 WHERE StudentId = 1 AND CourseId = 101;
EXCEPTION WHEN OTHERS THEN
    ROLLBACK TO SAVEPOINT sp1;
    RAISE NOTICE 'Ошибка: %', SQLERRM;

SAVEPOINT sp2;
UPDATE Marks SET Mark = 70 WHERE StudentId = 2 AND CourseId = 102;
EXCEPTION WHEN OTHERS THEN
    ROLLBACK TO SAVEPOINT sp2;
    RAISE NOTICE 'Ошибка: %', SQLERRM;

SAVEPOINT sp3;
UPDATE Marks SET Mark = 60 WHERE StudentId = 2 AND CourseId = 101;
EXCEPTION WHEN OTHERS THEN
    ROLLBACK TO SAVEPOINT sp3;
    RAISE NOTICE 'Ошибка: %', SQLERRM;

COMMIT;

-- Проверка результата — оценки не должны измениться
SELECT * FROM Marks ORDER BY StudentId, CourseId;
