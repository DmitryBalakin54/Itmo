
-- RegisterUserTest
-- SELECT RegisterUser(1, '123');
-- SELECT RegisterUser(2, 'abcde');
-- SELECT RegisterUser(2, 'abcdefg');
--
-- SELECT * FROM Users;

-- FindUserByPass
-- SELECT FindUserByPass(1, '123');
-- SELECT FindUserByPass(2, 'abcde');
-- SELECT FindUserByPass(2, 'abcdefg');
--
-- SELECT * FROM Users;

-- ManageFlight
-- TRUNCATE Users;
--
-- SELECT RegisterUser(1, '123');
--
-- INSERT INTO Flights (FlightId, FlightTime, PlaneId, SellAllowed, ReservationAllowed) VALUES
-- (1, CURRENT_TIMESTAMP + INTERVAL '5 days', 101, true, true),
-- (2, CURRENT_TIMESTAMP + INTERVAL '10 days', 102, true, true),
--
-- (3, CURRENT_TIMESTAMP + INTERVAL '2 days 12 hours', 103, true, true),
-- (4, CURRENT_TIMESTAMP + INTERVAL '3 days', 104, true, true),
--
-- (5, CURRENT_TIMESTAMP + INTERVAL '2 hours', 105, true, true),
-- (6, CURRENT_TIMESTAMP + INTERVAL '4 hours', 106, true, true),
--
-- (7, CURRENT_TIMESTAMP - INTERVAL '1 day', 107, true, true),
-- (8, CURRENT_TIMESTAMP - INTERVAL '5 hours', 108, true, true),
--
-- (9, CURRENT_TIMESTAMP + INTERVAL '7 days', 109, false, true),
-- (10, CURRENT_TIMESTAMP + INTERVAL '6 days', 110, true, false),
-- (11, CURRENT_TIMESTAMP + INTERVAL '8 days', 111, false, false);
--
-- SELECT ManageFlight(1, '123', 1, false, false);
-- SELECT ManageFlight(1, '1234', 1, true, true);
-- SELECT ManageFlight(10, '123', 1, true, false);
--
-- SELECT *
-- FROM Flights;

-- FreeSeats
-- TRUNCATE Users, Flights, Seats, Tickets;
--
-- INSERT INTO Flights (FlightId, FlightTime, PlaneId) VALUES
-- (1, CURRENT_TIMESTAMP + INTERVAL '5 days', 101),
-- (2, CURRENT_TIMESTAMP + INTERVAL '10 days', 102);
--
-- INSERT INTO Seats (PlaneId, SeatNo) VALUES
-- (101, '1A'), (101, '1B'), (101, '1C'), (101, '2A'), (101, '2B'),
-- (102, '1A'), (102, '1B'), (102, '1C');
--
-- INSERT INTO Users (UserId, PassHash) VALUES
-- (1, crypt('123', gen_salt('bf')));
--
-- SELECT '=== Тест 1: Все места свободны (рейс 1) ===';
-- SELECT * FROM FreeSeats(1);
--
-- SELECT '=== Тест 2: Все места свободны (рейс 2) ===';
-- SELECT * FROM FreeSeats(2);
--
-- INSERT INTO Tickets (FlightId, SeatNo, UserId, IsReserved, ExpireDate) VALUES
-- (1, '1A', 1, true, CURRENT_TIMESTAMP + INTERVAL '1 day'),
-- (1, '1B', 1, true, CURRENT_TIMESTAMP + INTERVAL '1 day'),
-- (2, '1A', 1, false, CURRENT_TIMESTAMP + INTERVAL '1 day');
--
-- SELECT '=== Тест 3: Некоторые места заняты (рейс 1) ===';
-- SELECT * FROM FreeSeats(1);
--
-- SELECT '=== Тест 4: Некоторые места заняты (рейс 2) ===';
-- SELECT * FROM FreeSeats(2);
--
-- INSERT INTO Tickets (FlightId, SeatNo, UserId, IsReserved, ExpireDate) VALUES
-- (1, '2A', 1, true, CURRENT_TIMESTAMP + INTERVAL '1 day');
--
-- SELECT '=== Тест 5: Больше занятых мест (рейс 1) ===';
-- SELECT * FROM FreeSeats(1);
--
-- SELECT '=== Тест 6: Несуществующий рейс (999) ===';
-- SELECT * FROM FreeSeats(999);
--
-- SELECT '=== Тест 7: Подсчет свободных мест ===';
-- SELECT COUNT(*) as free_seats_count FROM FreeSeats(1);
--
-- SELECT '=== Проверка исходных данных ===';
-- SELECT 'Flights:' as info; SELECT * FROM Flights;
-- SELECT 'Seats:' as info; SELECT * FROM Seats ORDER BY PlaneId, SeatNo;
-- SELECT 'Tickets:' as info; SELECT * FROM Tickets ORDER BY FlightId, SeatNo;

-- Reserve
-- TRUNCATE Users, Flights, Seats, Tickets;
--
-- SELECT RegisterUser(1, 'pass1');
-- SELECT RegisterUser(2, 'pass2');
--
-- INSERT INTO Flights (FlightId, FlightTime, PlaneId) VALUES
-- (1, CURRENT_TIMESTAMP + INTERVAL '5 days', 101),
-- (2, CURRENT_TIMESTAMP + INTERVAL '1 day', 102);
--
-- INSERT INTO Seats (PlaneId, SeatNo) VALUES
-- (101, '1A'), (101, '1B'),
-- (102, '2A');
--
-- SELECT '=== Test 1: Successful reservation ===';
-- SELECT Reserve(1, 'pass1', 1, '1A');
-- SELECT * FROM Tickets;
--
-- SELECT '=== Test 2: Wrong password ===';
-- SELECT Reserve(1, 'wrongpass', 1, '1B');
--
-- SELECT '=== Test 3: Non-existent user ===';
-- SELECT Reserve(999, 'pass1', 1, '1B');
--
-- SELECT '=== Test 4: Seat already taken ===';
-- SELECT Reserve(2, 'pass2', 1, '1A');
--
-- SELECT '=== Test 5: Another successful reservation ===';
-- SELECT Reserve(2, 'pass2', 1, '1B');
-- SELECT * FROM Tickets;
--
-- SELECT '=== Test 6: Non-existent flight ===';
-- SELECT Reserve(1, 'pass1', 999, '1A');
--
-- SELECT '=== Test 7: Non-existent seat ===';
-- SELECT Reserve(1, 'pass1', 1, '999Z');
--
-- SELECT '=== Test 8: Cannot reserve seat for flight departing in 1 day ===';
-- SELECT Reserve(1, 'pass1', 2, '2A');
-- SELECT * FROM Tickets;
--
-- SELECT '=== Final check ===';
-- SELECT * FROM Tickets ORDER BY FlightId, SeatNo;

-- ExtendReservation
-- TRUNCATE Users, Flights, Seats, Tickets;
--
-- SELECT RegisterUser(1, 'pass1');
-- SELECT RegisterUser(2, 'pass2');
--
-- INSERT INTO Flights (FlightId, FlightTime, PlaneId) VALUES
-- (1, CURRENT_TIMESTAMP + INTERVAL '5 days', 101),
-- (2, CURRENT_TIMESTAMP + INTERVAL '1 day', 102);
--
-- INSERT INTO Seats (PlaneId, SeatNo) VALUES
-- (101, '1A'), (101, '1B'), (101, '3A'),
-- (102, '2A');
--
-- SELECT Reserve(1, 'pass1', 1, '1A');
-- SELECT Reserve(2, 'pass2', 1, '1B');
--
-- SELECT '=== Test 1: Successful extension ===';
-- SELECT ExtendReservation(1, 'pass1', 1, '1A');
-- SELECT FlightId, SeatNo, UserId, ExpireDate FROM Tickets ORDER BY FlightId, SeatNo;
--
-- SELECT '=== Test 2: Wrong password ===';
-- SELECT ExtendReservation(1, 'wrongpass', 1, '1A');
--
-- SELECT '=== Test 3: Non-existent user ===';
-- SELECT ExtendReservation(999, 'pass1', 1, '1A');
--
-- SELECT '=== Test 4: Not your reservation ===';
-- SELECT ExtendReservation(1, 'pass1', 1, '1B');
--
-- SELECT '=== Test 5: Non-existent reservation ===';
-- SELECT ExtendReservation(1, 'pass1', 1, '999Z');
--
-- SELECT '=== Test 6: Cannot extend - flight departs in 1 day ===';
-- SELECT ExtendReservation(2, 'pass2', 2, '2A');
--
-- SELECT '=== Test 7: Try to extend purchased ticket ===';
-- INSERT INTO Tickets (FlightId, SeatNo, UserId, IsReserved, ExpireDate) VALUES
-- (1, '3A', 1, false, CURRENT_TIMESTAMP + INTERVAL '1 day');
-- SELECT ExtendReservation(1, 'pass1', 1, '3A');
--
-- SELECT '=== Final check ===';
-- SELECT FlightId, SeatNo, UserId, IsReserved, ExpireDate FROM Tickets ORDER BY FlightId, SeatNo;

-- BuyFree
-- TRUNCATE Users, Flights, Seats, Tickets;
--
-- INSERT INTO Flights (FlightId, FlightTime, PlaneId) VALUES
-- (1, CURRENT_TIMESTAMP + INTERVAL '5 days', 101),
-- (2, CURRENT_TIMESTAMP + INTERVAL '2 hours', 102),
-- (3, CURRENT_TIMESTAMP + INTERVAL '1 hour', 103);
--
-- INSERT INTO Seats (PlaneId, SeatNo) VALUES
-- (101, '1A'), (101, '1B'), (101, '1C'),
-- (102, '2A'), (102, '2B'),
-- (103, '3A');
--
-- SELECT '=== Test 1: Successful purchase of free seat ===';
-- SELECT BuyFree(1, '1A');
-- SELECT * FROM Tickets WHERE FlightId = 1;
--
-- SELECT '=== Test 2: Purchase another free seat ===';
-- SELECT BuyFree(1, '1B');
-- SELECT * FROM Tickets WHERE FlightId = 1;
--
-- SELECT '=== Test 3: Cannot purchase already taken seat ===';
-- SELECT BuyFree(1, '1A');
--
-- SELECT '=== Test 4: Cannot purchase non-existent seat ===';
-- SELECT BuyFree(1, '999Z');
--
-- SELECT '=== Test 5: Cannot purchase - flight departs in 2 hours (within 3 hours limit) ===';
-- SELECT BuyFree(2, '2A');
--
-- SELECT '=== Test 6: Cannot purchase - flight departs in 1 hour ===';
-- SELECT BuyFree(3, '3A');
--
-- SELECT '=== Test 7: Cannot purchase non-existent flight ===';
-- SELECT BuyFree(999, '1A');
--
-- SELECT '=== Test 8: Purchase with expired reservation should work ===';
-- SELECT RegisterUser(1, 'pass1');
-- SELECT Reserve(1, 'pass1', 1, '1C');
-- UPDATE Tickets SET ExpireDate = CURRENT_TIMESTAMP - INTERVAL '1 hour' WHERE FlightId = 1 AND SeatNo = '1C';
-- SELECT BuyFree(1, '1C');
-- SELECT * FROM Tickets WHERE FlightId = 1 AND SeatNo = '1C';
--
-- SELECT '=== Final check ===';
-- SELECT FlightId, SeatNo, UserId, IsReserved, ExpireDate FROM Tickets ORDER BY FlightId, SeatNo;

-- BuyReserved
-- TRUNCATE Users, Flights, Seats, Tickets;
--
-- SELECT RegisterUser(1, 'pass1');
-- SELECT RegisterUser(2, 'pass2');
--
-- INSERT INTO Flights (FlightId, FlightTime, PlaneId) VALUES
-- (1, CURRENT_TIMESTAMP + INTERVAL '5 days', 101),
-- (2, CURRENT_TIMESTAMP + INTERVAL '2 hours', 102),
-- (3, CURRENT_TIMESTAMP + INTERVAL '4 days', 103);
--
-- INSERT INTO Seats (PlaneId, SeatNo) VALUES
-- (101, '1A'), (101, '1B'), (101, '1C'),
-- (102, '2A'), (102, '2B'),
-- (103, '3A');
--
-- SELECT Reserve(1, 'pass1', 1, '1A');
-- SELECT Reserve(2, 'pass2', 1, '1B');
-- SELECT Reserve(1, 'pass1', 3, '3A');
--
-- SELECT '=== Test 1: Successful purchase of reserved seat ===';
-- SELECT BuyReserved(1, 'pass1', 1, '1A');
-- SELECT FlightId, SeatNo, UserId, IsReserved FROM Tickets WHERE FlightId = 1 AND SeatNo = '1A';
--
-- SELECT '=== Test 2: Wrong password ===';
-- SELECT BuyReserved(1, 'wrongpass', 1, '1B');
--
-- SELECT '=== Test 3: Not your reservation ===';
-- SELECT BuyReserved(1, 'pass1', 1, '1B');
--
-- SELECT '=== Test 4: Non-existent reservation ===';
-- SELECT BuyReserved(1, 'pass1', 1, '1C');
--
-- SELECT '=== Test 5: Cannot purchase - flight departs in 2 hours ===';
-- SELECT BuyReserved(2, 'pass2', 2, '2A');
--
-- SELECT '=== Test 6: Non-existent user ===';
-- SELECT BuyReserved(999, 'pass1', 1, '1B');
--
-- SELECT '=== Test 7: Non-existent flight ===';
-- SELECT BuyReserved(1, 'pass1', 999, '1A');
--
-- SELECT '=== Test 8: Cannot purchase expired reservation ===';
-- UPDATE Tickets SET ExpireDate = CURRENT_TIMESTAMP - INTERVAL '1 hour' WHERE FlightId = 1 AND SeatNo = '1B';
-- SELECT BuyReserved(2, 'pass2', 1, '1B');
--
-- SELECT '=== Test 9: Another successful purchase ===';
-- SELECT BuyReserved(1, 'pass1', 3, '3A');
-- SELECT FlightId, SeatNo, UserId, IsReserved FROM Tickets WHERE FlightId = 3;
--
-- SELECT '=== Final check ===';
-- SELECT FlightId, SeatNo, UserId, IsReserved, ExpireDate FROM Tickets ORDER BY FlightId, SeatNo;

-- FlightsStatistics
-- TRUNCATE Users, Flights, Seats, Tickets;
--
-- SELECT RegisterUser(1, 'pass1');
-- SELECT RegisterUser(2, 'pass2');
--
-- INSERT INTO Flights (FlightId, FlightTime, PlaneId) VALUES
-- (1, CURRENT_TIMESTAMP + INTERVAL '5 days', 101),
-- (2, CURRENT_TIMESTAMP + INTERVAL '2 days', 102),
-- (3, CURRENT_TIMESTAMP + INTERVAL '2 hours', 103),
-- (4, CURRENT_TIMESTAMP + INTERVAL '1 day', 104);
--
-- INSERT INTO Seats (PlaneId, SeatNo) VALUES
-- (101, '1A'), (101, '1B'), (101, '1C'),
-- (102, '2A'), (102, '2B'),
-- (103, '3A'), (103, '3B'),
-- (104, '4A'), (104, '4B'), (104, '4C');
--
-- SELECT Reserve(1, 'pass1', 1, '1A');
-- SELECT Reserve(1, 'pass1', 1, '1B');
-- SELECT BuyFree(1, '1C');
-- SELECT Reserve(2, 'pass2', 2, '2A');
-- SELECT BuyFree(3, '3A');
--
-- SELECT '=== Test 1: Successful statistics for user 1 ===';
-- SELECT * FROM FlightsStatistics(1, 'pass1');
--
-- SELECT '=== Test 2: Wrong password ===';
-- SELECT * FROM FlightsStatistics(1, 'wrongpass');
--
-- SELECT '=== Test 3: Non-existent user ===';
-- SELECT * FROM FlightsStatistics(999, 'pass1');
--
-- SELECT '=== Test 4: Statistics for user 2 ===';
-- SELECT * FROM FlightsStatistics(2, 'pass2');
--
-- SELECT '=== Test 5: Add more reservations and purchases ===';
-- SELECT Reserve(1, 'pass1', 4, '4A');
-- SELECT BuyReserved(1, 'pass1', 4, '4A');
-- SELECT * FROM FlightsStatistics(1, 'pass1') WHERE FlightId = 4;
--
-- SELECT '=== Test 6: Check flight 3 (departs in 2 hours) ===';
-- SELECT * FROM FlightsStatistics(1, 'pass1') WHERE FlightId = 3;
--
-- SELECT '=== Test 7: Check flight 2 (departs in 2 days) ===';
-- SELECT * FROM FlightsStatistics(1, 'pass1') WHERE FlightId = 2;
--
-- SELECT '=== Final detailed check ===';
-- SELECT
--     F.FlightId,
--     F.FlightTime,
--     S.CanBuy,
--     S.CanReserve,
--     S.FreeSeatsCount,
--     S.ReservedSeatsCount,
--     S.SoldSeatsCount
-- FROM Flights F
-- JOIN FlightsStatistics(1, 'pass1') S ON F.FlightId = S.FlightId
-- ORDER BY F.FlightId;

-- CompressSeats
TRUNCATE Users, Flights, Seats, Tickets;

SELECT RegisterUser(1, 'pass1');
UPDATE Users SET IsAdmin = true WHERE UserId = 1;
SELECT RegisterUser(2, 'pass2');
SELECT RegisterUser(3, 'pass3');

INSERT INTO Flights (FlightId, FlightTime, PlaneId) VALUES
(1, CURRENT_TIMESTAMP + INTERVAL '5 days', 101),
(2, CURRENT_TIMESTAMP + INTERVAL '5 days', 102);

INSERT INTO Seats (PlaneId, SeatNo) VALUES
(101, '1A'), (101, '1B'), (101, '1C'), (101, '2A'), (101, '2B'), (101, '2C'), (101, '3A'), (101, '3B'), (101, '3C'),
(102, '1A'), (102, '1B'), (102, '1C'), (102, '2A'), (102, '2B'), (102, '2C');

-- SELECT '=== Test 1: Mixed seats (purchased, reserved, free) ===';
-- SELECT BuyFree(1, '2B');
-- SELECT Reserve(1, 'pass1', 1, '1C');
-- SELECT Reserve(2, 'pass2', 1, '3A');
-- SELECT BuyFree(1, '2A');
--
-- SELECT 'Seats before compression:';
-- SELECT T.FlightId, T.SeatNo, T.UserId, T.IsReserved
-- FROM Tickets T WHERE T.FlightId = 1
-- ORDER BY T.SeatNo;
--
-- SELECT 'Free seats before compression:';
-- SELECT * FROM FreeSeats(1) ORDER BY 1;
--
-- CALL CompressSeats(1, 'pass1', 1);
--
-- SELECT 'Seats after compression:';
-- SELECT T.FlightId, T.SeatNo, T.UserId, T.IsReserved
-- FROM Tickets T WHERE T.FlightId = 1
-- ORDER BY T.SeatNo;
--
-- SELECT 'Free seats after compression:';
-- SELECT * FROM FreeSeats(1) ORDER BY 1;
--
-- SELECT '=== Test 2: All seats purchased ===';
-- SELECT BuyFree(2, '1A');
-- SELECT BuyFree(2, '1B');
-- SELECT BuyFree(2, '1C');
-- SELECT BuyFree(2, '2A');
-- SELECT BuyFree(2, '2B');
-- SELECT BuyFree(2, '2C');
--
-- SELECT 'Seats before compression (flight 2):';
-- SELECT T.FlightId, T.SeatNo, T.UserId, T.IsReserved
-- FROM Tickets T WHERE T.FlightId = 2
-- ORDER BY T.SeatNo;
--
-- CALL CompressSeats(1, 'pass1', 2);
--
-- SELECT 'Seats after compression (flight 2):';
-- SELECT T.FlightId, T.SeatNo, T.UserId, T.IsReserved
-- FROM Tickets T WHERE T.FlightId = 2
-- ORDER BY T.SeatNo;
--
-- SELECT '=== Test 3: All seats reserved ===';
-- TRUNCATE Tickets;
-- SELECT Reserve(1, 'pass1', 1, '1A');
-- SELECT Reserve(2, 'pass2', 1, '1B');
-- SELECT Reserve(3, 'pass3', 1, '1C');
-- SELECT Reserve(1, 'pass1', 1, '2A');
-- SELECT Reserve(2, 'pass2', 1, '2B');
--
-- SELECT 'Seats before compression (flight 1 - all reserved):';
-- SELECT T.FlightId, T.SeatNo, T.UserId, T.IsReserved
-- FROM Tickets T WHERE T.FlightId = 1
-- ORDER BY T.SeatNo;
--
-- CALL CompressSeats(1, 'pass1', 1);
--
-- SELECT 'Seats after compression (flight 1 - all reserved):';
-- SELECT T.FlightId, T.SeatNo, T.UserId, T.IsReserved
-- FROM Tickets T WHERE T.FlightId = 1
-- ORDER BY T.SeatNo;
--
-- SELECT '=== Test 4: All seats free ===';
-- TRUNCATE Tickets;
--
-- SELECT 'Seats before compression (flight 1 - all free):';
-- SELECT * FROM FreeSeats(1) ORDER BY 1;
--
-- CALL CompressSeats(1, 'pass1', 1);
--
-- SELECT 'Seats after compression (flight 1 - all free):';
-- SELECT * FROM FreeSeats(1) ORDER BY 1;
--
-- SELECT '=== Test 5: Wrong user password ===';
-- CALL CompressSeats(1, 'wrongpass', 1);
--
-- SELECT '=== Test 6: Non-existent flight ===';
-- CALL CompressSeats(1, 'pass1', 999);
--
-- SELECT '=== Test 7: Non-existent user ===';
-- CALL CompressSeats(999, 'pass1', 1);


SELECT '=== Test 8: Mixed seats (purchased, reserved, free) ===';
SELECT Reserve(3, 'pass3', 1,'2B');
SELECT Reserve(1, 'pass1', 1, '1C');
SELECT Reserve(2, 'pass2', 1, '3A');
SELECT Reserve(3, 'pass3', 1, '2A');
SELECT Reserve(1, 'pass1', 1, '2C');

SELECT BuyReserved(3, 'pass3', 1,'2B');
SELECT BuyReserved(1, 'pass1', 1, '1C');
SELECT BuyReserved(2, 'pass2', 1, '3A');


SELECT 'Seats before compression:';
SELECT T.FlightId, T.SeatNo, T.UserId, T.IsReserved
FROM Tickets T WHERE T.FlightId = 1
ORDER BY T.SeatNo;

SELECT 'Free seats before compression:';
SELECT * FROM FreeSeats(1) ORDER BY 1;

CALL CompressSeats(1, 'pass1', 1);

SELECT 'Seats after compression:';
SELECT T.FlightId, T.SeatNo, T.UserId, T.IsReserved
FROM Tickets T WHERE T.FlightId = 1
ORDER BY T.SeatNo;

SELECT 'Free seats after compression:';
SELECT * FROM FreeSeats(1) ORDER BY 1;