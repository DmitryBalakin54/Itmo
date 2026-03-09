INSERT INTO People (Id, FirstName, Surname, Passport) VALUES
(1, 'Ivan', 'Petrov', 'P123456789'),
(2, 'Olga', 'Sidorova', 'P987654321'),
(3, 'Pavel', 'Ivanov', 'P111222333'),
(4, 'Anna', 'Kuznetsova', 'P444555666'),
(5, 'Dmitry', 'Smirnov', 'P777888999'),
(6, 'Marina', 'Lebedeva', 'P000111222'),
(7, 'Sergey', 'Volkov', 'P333444555'),
(8, 'Natalia', 'Morozova', 'P666777888'),
(9, 'Andrey', 'Fedorov', 'P999000111'),
(10, 'Elena', 'Popova', 'P222333444');

INSERT INTO Groups (Id, Name) VALUES
(10, 'M3132'),
(20, 'M3133'),
(30, 'M3134'),
(40, 'M3135'),
(50, 'M3136'),
(60, 'M3137'),
(70, 'M3138'),
(80, 'M3139');

INSERT INTO Student (StudentId, GroupId) VALUES
(4, 10),
(5, 20),
(6, 30),
(8, 20),
(10, 40);

INSERT INTO Course (Id, Name, Description) VALUES
(100, 'Mathematics', 'Basic mathematics course'),
(200, 'Physics', 'Introductory physics'),
(300, 'AISD', 'Algorithms and data structures'),
(400, 'History', 'World history overview');

INSERT INTO TeacherCourse (TeacherId, CourseId) VALUES
(2, 100),
(3, 200),
(7, 300),
(9, 400);

INSERT INTO GroupsCourse (CourseId, GroupId, TeacherId) VALUES
(100, 10, 2),
(200, 20, 3),
(300, 30, 7),
(400, 40, 9),
(300, 50, 7);

INSERT INTO Club (Id, Name, Description, TutorId) VALUES
(300, 'ChessClub', 'Club for chess enthusiasts', 1),
(400, 'DramaClub', 'Theatre and acting', 2),
(500, 'ProgrammingClub', 'Algorithms and coding', 7);

INSERT INTO Member (MemberId, ClubId) VALUES
(4, 300),
(5, 300),
(6, 400),
(8, 400),
(10, 300),
(10, 400),
(9, 500),
(7, 500);
