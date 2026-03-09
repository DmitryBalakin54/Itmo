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

-- Тестируем валидные обновления (увеличение или оставление оценки)
UPDATE Marks SET Mark = 95 WHERE StudentId = 1 AND CourseId = 101;  -- увеличение
UPDATE Marks SET Mark = 90 WHERE StudentId = 1 AND CourseId = 102;  -- оставляем прежнее значение
UPDATE Marks SET Mark = 85 WHERE StudentId = 2 AND CourseId = 101;  -- оставляем прежнее значение
UPDATE Marks SET Mark = 85 WHERE StudentId = 2 AND CourseId = 102;  -- увеличение

-- Проверка результата
SELECT * FROM Marks ORDER BY StudentId, CourseId;
