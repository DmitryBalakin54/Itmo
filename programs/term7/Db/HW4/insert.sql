INSERT INTO Groups VALUES
(10, 'M3132'),
(20, 'M3133'),
(30, 'M3134'),
(40, 'M3135'),
(50, 'M3136'),
(60, 'M3137'),
(70, 'M3138'),
(80, 'M3139');

INSERT INTO Student (StudentId, StudentName, GroupId) VALUES
(1, 'Ivan Petrov', 10),
(2, 'Olga Sidorova', 20),
(3, 'Pavel Ivanov', 30),
(4, 'Anna Kuznetsova', 10),
(5, 'Dmitry Smirnov', 20),
(6, 'Marina Lebedeva', 30),
(7, 'Sergey Volkov', 50),
(8, 'Natalia Morozova', 20),
(9, 'Andrey Fedorov', 40),
(10, 'Elena Popova', 40);

INSERT INTO Lecturer (LecturerId, LecturerName) VALUES
(1, 'Ivan Petrov'),
(2, 'Olga Sidorova'),
(3, 'Pavel Ivanov'),
(4, 'Anna Kuznetsova'),
(5, 'Dmitry Smirnov'),
(6, 'Marina Lebedeva'),
(7, 'Sergey Volkov'),
(8, 'Natalia Morozova'),
(9, 'Andrey Fedorov'),
(10, 'Elena Popova');

INSERT INTO Course (CourseId, CourseName) VALUES
(100, 'Mathematics'),
(200, 'Physics'),
(300, 'AISD'),
(400, 'History');

INSERT INTO GroupCourse (CourseId, GroupId, LecturerId) VALUES
(100, 10, 2),
(200, 20, 3),
(300, 30, 7),
(400, 40, 9),
(300, 50, 7);

INSERT INTO Mark (Mark, StudentId, CourseId) VALUES
('A', 4, 100),
('B', 5, 200),
('C', 6, 300),
('A', 8, 200),
('B', 10, 400);

INSERT INTO Club (ClubId, ClubName, ClubStudentHeadId) VALUES
(300, 'ChessClub', 1),
(400, 'DramaClub', 2),
(500, 'ProgrammingClub', 7);
