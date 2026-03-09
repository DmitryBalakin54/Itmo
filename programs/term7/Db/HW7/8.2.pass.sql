TRUNCATE Groups, Students, Lecturers, Courses, Plan, Marks RESTART IDENTITY CASCADE;

INSERT INTO Groups (GroupId, GroupName) VALUES
(1, 'G1'),
(2, 'G2');

INSERT INTO Students (StudentId, StudentName, GroupId) VALUES
(1, 'Иванов', 1),
(2, 'Петров', 1),
(3, 'Сидоров', 2),
(4, 'Кузнецов', 2);

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
(3, 103, 88),
(3, 104, 92),
(4, 103, 95),
(4, 104, 89);

SELECT * FROM Marks ORDER BY StudentId, CourseId;
