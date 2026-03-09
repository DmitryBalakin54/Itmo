INSERT INTO Groups (GroupId, GroupName) VALUES
(1, 'CS-101'),
(2, 'CS-102'),
(3, 'MATH-101'),
(4, 'PHYS-101');

INSERT INTO Students (StudentId, StudentName, GroupId) VALUES
(1, 'Иван Иванов', 1),
(2, 'Петр Петров', 1),
(3, 'Мария Сидорова', 2),
(4, 'Анна Козлова', 2),
(5, 'Сергей Смирнов', 3),
(6, 'Ольга Орлова', 3),
(7, 'Дмитрий Волков', 4),
(8, 'Елена Новикова', 4);

INSERT INTO Lecturers (LecturerId, LecturerName) VALUES
(1, 'Профессор Александров'),
(2, 'Доцент Белова'),
(3, 'Профессор Григорьев'),
(4, 'Старший преподаватель Дмитриева');

INSERT INTO Courses (CourseId, CourseName) VALUES
(1, 'Математический анализ'),
(2, 'Программирование'),
(3, 'Физика'),
(4, 'Базы данных'),
(5, 'Алгоритмы и структуры данных');

INSERT INTO Plan (GroupId, CourseId, LecturerId) VALUES
(1, 1, 1), (1, 2, 2), (1, 4, 4),
(2, 1, 1), (2, 2, 2), (2, 5, 3),
(3, 1, 1), (3, 3, 3),
(4, 1, 1), (4, 3, 3), (4, 5, 3);

INSERT INTO Marks (StudentId, CourseId, Mark) VALUES
(1, 1, 4), (1, 2, 5), (1, 4, 3),
(2, 1, 3), (2, 2, 4),
(3, 1, 5), (3, 2, 5), (3, 5, 4),
(4, 1, 4), (4, 2, 3),
(5, 1, 5), (5, 3, 4),
(6, 1, 3), (6, 3, 5),
(7, 1, 4), (7, 3, 3), (7, 5, 4),
(8, 1, 5), (8, 3, 5), (8, 5, 5);


INSERT INTO Clubs (ClubId, ClubName, ClubStudentHeadId) VALUES
(1, 'Программирование', 1),
(2, 'Математический кружок', 5),
(3, 'Физическая лаборатория', 7);

INSERT INTO ClubMembers (ClubId, StudentId) VALUES
(1, 1), (1, 2), (1, 3),
(2, 5), (2, 6), (2, 1),
(3, 7), (3, 8), (3, 4);