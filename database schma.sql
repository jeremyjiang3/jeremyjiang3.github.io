/*
Run this script on:

        new check  -  This database will be modified

to synchronize it with:

        is-swang01.ischool.uw.edu,1435.train

You are recommended to back up your database before running this script

Script created by SQL Compare version 14.0.0.12866 from Red Gate Software Ltd at 2019/12/3 15:46:11

*/
SET NUMERIC_ROUNDABORT OFF
GO
SET ANSI_PADDING, ANSI_WARNINGS, CONCAT_NULL_YIELDS_NULL, ARITHABORT, QUOTED_IDENTIFIER, ANSI_NULLS ON
GO
SET XACT_ABORT ON
GO
SET TRANSACTION ISOLATION LEVEL Serializable
GO
BEGIN TRANSACTION
GO
IF @@ERROR <> 0 SET NOEXEC ON
GO
PRINT N'Creating [dbo].[Passenger]'
GO
CREATE TABLE [dbo].[Passenger]
(
[idPassenger] [int] NOT NULL,
[gender] [varchar] (10) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
[age] [decimal] (2, 0) NULL,
[first_name] [varchar] (50) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
[last_name] [varchar] (50) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
[dob] [datetime] NULL,
[nationality] [varchar] (50) COLLATE SQL_Latin1_General_CP1_CI_AS NULL
)
GO
IF @@ERROR <> 0 SET NOEXEC ON
GO
PRINT N'Creating primary key [PK__Passenge__89EDE752151873AF] on [dbo].[Passenger]'
GO
ALTER TABLE [dbo].[Passenger] ADD CONSTRAINT [PK__Passenge__89EDE752151873AF] PRIMARY KEY CLUSTERED  ([idPassenger])
GO
IF @@ERROR <> 0 SET NOEXEC ON
GO
PRINT N'Creating trigger [dbo].[uf_getpassengerage] on [dbo].[Passenger]'
GO
CREATE TRIGGER [dbo].[uf_getpassengerage]
ON [dbo].[Passenger]
AFTER INSERT,update,delete
AS 
BEGIN 
	DECLARE @idpass int

	select @idpass=isnull(i.idpassenger,d.idpassenger)
	from inserted i full outer join deleted d 
	on i.idPassenger=d.idPassenger;

update dbo.passenger
set [Age]=DateDiff(hour,[DOB],getdate())/8766 
where idPassenger=@idpass

END;
GO
IF @@ERROR <> 0 SET NOEXEC ON
GO
PRINT N'Creating [dbo].[Online_account]'
GO
CREATE TABLE [dbo].[Online_account]
(
[idOnline_account] [int] NOT NULL IDENTITY(1, 1),
[user_name] [varchar] (100) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
[email] [varchar] (50) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
[password] [varchar] (100) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
[mobile_number] [varchar] (15) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL,
[payment_idOnline_account] [int] NOT NULL
)
GO
IF @@ERROR <> 0 SET NOEXEC ON
GO
PRINT N'Creating primary key [PK__Online_a__B9B95EF5BEF95550] on [dbo].[Online_account]'
GO
ALTER TABLE [dbo].[Online_account] ADD CONSTRAINT [PK__Online_a__B9B95EF5BEF95550] PRIMARY KEY CLUSTERED  ([idOnline_account])
GO
IF @@ERROR <> 0 SET NOEXEC ON
GO
PRINT N'Creating [dbo].[Account_security]'
GO
CREATE TABLE [dbo].[Account_security]
(
[security_question] [nvarchar] (250) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
[retrieve_email] [varchar] (50) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
[IDaccount_security] [int] NOT NULL,
[Online_account_idOnline_account] [int] NOT NULL
)
GO
IF @@ERROR <> 0 SET NOEXEC ON
GO
PRINT N'Creating primary key [PK__Account___89288865DFB2157A] on [dbo].[Account_security]'
GO
ALTER TABLE [dbo].[Account_security] ADD CONSTRAINT [PK__Account___89288865DFB2157A] PRIMARY KEY CLUSTERED  ([IDaccount_security])
GO
IF @@ERROR <> 0 SET NOEXEC ON
GO
PRINT N'Creating [dbo].[Ticket]'
GO
CREATE TABLE [dbo].[Ticket]
(
[idTicket] [int] NOT NULL,
[seat_number] [varchar] (45) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
[Train_classType_id] [int] NULL,
[Ticket_Type_idTicket_Type] [int] NOT NULL,
[Online_account_idOnline_account] [int] NOT NULL
)
GO
IF @@ERROR <> 0 SET NOEXEC ON
GO
PRINT N'Creating primary key [PK__Ticket__22B1456F27A101F4] on [dbo].[Ticket]'
GO
ALTER TABLE [dbo].[Ticket] ADD CONSTRAINT [PK__Ticket__22B1456F27A101F4] PRIMARY KEY CLUSTERED  ([idTicket])
GO
IF @@ERROR <> 0 SET NOEXEC ON
GO
PRINT N'Creating [dbo].[cancellation_status]'
GO
CREATE TABLE [dbo].[cancellation_status]
(
[cancelled_date] [datetime] NULL,
[cancelled_status] [varchar] (10) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
[requested_date] [datetime] NULL,
[idTicket_cancel] [int] NOT NULL,
[Ticket_idTicket] [int] NOT NULL
)
GO
IF @@ERROR <> 0 SET NOEXEC ON
GO
PRINT N'Creating primary key [PK__cancella__414E1E15DAA1E246] on [dbo].[cancellation_status]'
GO
ALTER TABLE [dbo].[cancellation_status] ADD CONSTRAINT [PK__cancella__414E1E15DAA1E246] PRIMARY KEY CLUSTERED  ([idTicket_cancel])
GO
IF @@ERROR <> 0 SET NOEXEC ON
GO
PRINT N'Creating [dbo].[Comment]'
GO
CREATE TABLE [dbo].[Comment]
(
[service_rating] [varchar] (50) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
[idComment] [int] NOT NULL,
[Online_account_idOnline_account] [int] NOT NULL
)
GO
IF @@ERROR <> 0 SET NOEXEC ON
GO
PRINT N'Creating primary key [PK__Comment__0370297EA18BC792] on [dbo].[Comment]'
GO
ALTER TABLE [dbo].[Comment] ADD CONSTRAINT [PK__Comment__0370297EA18BC792] PRIMARY KEY CLUSTERED  ([idComment])
GO
IF @@ERROR <> 0 SET NOEXEC ON
GO
PRINT N'Creating [dbo].[Address]'
GO
CREATE TABLE [dbo].[Address]
(
[idaddress] [int] NOT NULL,
[Address] [varchar] (50) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
[zip_code] [varchar] (10) COLLATE SQL_Latin1_General_CP1_CI_AS NULL
)
GO
IF @@ERROR <> 0 SET NOEXEC ON
GO
PRINT N'Creating primary key [PK__Address__C8C1718546444EB5] on [dbo].[Address]'
GO
ALTER TABLE [dbo].[Address] ADD CONSTRAINT [PK__Address__C8C1718546444EB5] PRIMARY KEY CLUSTERED  ([idaddress])
GO
IF @@ERROR <> 0 SET NOEXEC ON
GO
PRINT N'Creating [dbo].[PassengerAndAddress]'
GO
CREATE TABLE [dbo].[PassengerAndAddress]
(
[idPassengerAndAddress] [int] NOT NULL,
[Address_idaddress] [int] NOT NULL,
[Passenger_idPassenger] [int] NOT NULL
)
GO
IF @@ERROR <> 0 SET NOEXEC ON
GO
PRINT N'Creating primary key [PK__Passenge__D09D75FDF3D25FB2] on [dbo].[PassengerAndAddress]'
GO
ALTER TABLE [dbo].[PassengerAndAddress] ADD CONSTRAINT [PK__Passenge__D09D75FDF3D25FB2] PRIMARY KEY CLUSTERED  ([idPassengerAndAddress])
GO
IF @@ERROR <> 0 SET NOEXEC ON
GO
PRINT N'Creating [dbo].[Agent]'
GO
CREATE TABLE [dbo].[Agent]
(
[idAgent] [int] NOT NULL,
[gender] [varchar] (10) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
[dob] [datetime] NULL,
[first_name] [varchar] (45) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
[last_name] [varchar] (45) COLLATE SQL_Latin1_General_CP1_CI_AS NULL
)
GO
IF @@ERROR <> 0 SET NOEXEC ON
GO
PRINT N'Creating primary key [PK__Agent__D87D391F7000C908] on [dbo].[Agent]'
GO
ALTER TABLE [dbo].[Agent] ADD CONSTRAINT [PK__Agent__D87D391F7000C908] PRIMARY KEY CLUSTERED  ([idAgent])
GO
IF @@ERROR <> 0 SET NOEXEC ON
GO
PRINT N'Creating [dbo].[PassengerAndAgent]'
GO
CREATE TABLE [dbo].[PassengerAndAgent]
(
[idPassengerAndAgent] [int] NOT NULL,
[Agent_idAgent] [int] NOT NULL,
[Passenger_idPassenger] [int] NOT NULL
)
GO
IF @@ERROR <> 0 SET NOEXEC ON
GO
PRINT N'Creating primary key [PK__Passenge__EF053405D980D672] on [dbo].[PassengerAndAgent]'
GO
ALTER TABLE [dbo].[PassengerAndAgent] ADD CONSTRAINT [PK__Passenge__EF053405D980D672] PRIMARY KEY CLUSTERED  ([idPassengerAndAgent])
GO
IF @@ERROR <> 0 SET NOEXEC ON
GO
PRINT N'Creating [dbo].[payment]'
GO
CREATE TABLE [dbo].[payment]
(
[idPayment] [int] NOT NULL,
[paymenthod_1] [varchar] (45) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
[Online_account_idOnline_account] [int] NOT NULL
)
GO
IF @@ERROR <> 0 SET NOEXEC ON
GO
PRINT N'Creating primary key [PK__payment__F22D4A457222E4F7] on [dbo].[payment]'
GO
ALTER TABLE [dbo].[payment] ADD CONSTRAINT [PK__payment__F22D4A457222E4F7] PRIMARY KEY CLUSTERED  ([idPayment])
GO
IF @@ERROR <> 0 SET NOEXEC ON
GO
PRINT N'Creating [dbo].[Register]'
GO
CREATE TABLE [dbo].[Register]
(
[Passenger_idPassenger] [int] NOT NULL,
[Online_account_idOnline_account] [int] NOT NULL
)
GO
IF @@ERROR <> 0 SET NOEXEC ON
GO
PRINT N'Creating primary key [PK__Register__79A7C6BF993467B2] on [dbo].[Register]'
GO
ALTER TABLE [dbo].[Register] ADD CONSTRAINT [PK__Register__79A7C6BF993467B2] PRIMARY KEY CLUSTERED  ([Passenger_idPassenger], [Online_account_idOnline_account])
GO
IF @@ERROR <> 0 SET NOEXEC ON
GO
PRINT N'Creating [dbo].[reservation_status]'
GO
CREATE TABLE [dbo].[reservation_status]
(
[reservation_date] [datetime] NULL,
[reservation_status] [varchar] (45) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
[idreservation_status] [int] NOT NULL,
[Ticket_idTicket] [int] NOT NULL
)
GO
IF @@ERROR <> 0 SET NOEXEC ON
GO
PRINT N'Creating primary key [PK__reservat__73A3F8D53052B68A] on [dbo].[reservation_status]'
GO
ALTER TABLE [dbo].[reservation_status] ADD CONSTRAINT [PK__reservat__73A3F8D53052B68A] PRIMARY KEY CLUSTERED  ([idreservation_status])
GO
IF @@ERROR <> 0 SET NOEXEC ON
GO
PRINT N'Creating [dbo].[Station]'
GO
CREATE TABLE [dbo].[Station]
(
[idStation] [int] NOT NULL,
[station_name] [varchar] (50) COLLATE SQL_Latin1_General_CP1_CI_AS NULL
)
GO
IF @@ERROR <> 0 SET NOEXEC ON
GO
PRINT N'Creating primary key [PK__Station__CED5EF153288B5F9] on [dbo].[Station]'
GO
ALTER TABLE [dbo].[Station] ADD CONSTRAINT [PK__Station__CED5EF153288B5F9] PRIMARY KEY CLUSTERED  ([idStation])
GO
IF @@ERROR <> 0 SET NOEXEC ON
GO
PRINT N'Creating [dbo].[Route]'
GO
CREATE TABLE [dbo].[Route]
(
[idTrain] [int] NOT NULL,
[idStation] [int] NOT NULL
)
GO
IF @@ERROR <> 0 SET NOEXEC ON
GO
PRINT N'Creating primary key [PK__Route__4E7E442DE9F75503] on [dbo].[Route]'
GO
ALTER TABLE [dbo].[Route] ADD CONSTRAINT [PK__Route__4E7E442DE9F75503] PRIMARY KEY CLUSTERED  ([idTrain], [idStation])
GO
IF @@ERROR <> 0 SET NOEXEC ON
GO
PRINT N'Creating [dbo].[Train]'
GO
CREATE TABLE [dbo].[Train]
(
[idTrain] [int] NOT NULL,
[train_name] [varchar] (50) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
[orientating_station] [varchar] (50) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
[destination_station] [varchar] (50) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
[train_arrtivaltime] [datetime] NULL,
[train_departuretime] [datetime] NULL
)
GO
IF @@ERROR <> 0 SET NOEXEC ON
GO
PRINT N'Creating primary key [PK__Train__02931ADC8024A38E] on [dbo].[Train]'
GO
ALTER TABLE [dbo].[Train] ADD CONSTRAINT [PK__Train__02931ADC8024A38E] PRIMARY KEY CLUSTERED  ([idTrain])
GO
IF @@ERROR <> 0 SET NOEXEC ON
GO
PRINT N'Creating [dbo].[Ticket_Type]'
GO
CREATE TABLE [dbo].[Ticket_Type]
(
[idTicket_Type] [int] NOT NULL,
[TicketTypeFare] [int] NULL
)
GO
IF @@ERROR <> 0 SET NOEXEC ON
GO
PRINT N'Creating primary key [PK__Ticket_T__FFED3DD34592F484] on [dbo].[Ticket_Type]'
GO
ALTER TABLE [dbo].[Ticket_Type] ADD CONSTRAINT [PK__Ticket_T__FFED3DD34592F484] PRIMARY KEY CLUSTERED  ([idTicket_Type])
GO
IF @@ERROR <> 0 SET NOEXEC ON
GO
PRINT N'Creating [dbo].[Train_classType]'
GO
CREATE TABLE [dbo].[Train_classType]
(
[idTrain_classType] [int] NOT NULL,
[TrainTypeName] [varchar] (45) COLLATE SQL_Latin1_General_CP1_CI_AS NULL
)
GO
IF @@ERROR <> 0 SET NOEXEC ON
GO
PRINT N'Creating primary key [PK__Train_cl__7C134B02A6A79127] on [dbo].[Train_classType]'
GO
ALTER TABLE [dbo].[Train_classType] ADD CONSTRAINT [PK__Train_cl__7C134B02A6A79127] PRIMARY KEY CLUSTERED  ([idTrain_classType])
GO
IF @@ERROR <> 0 SET NOEXEC ON
GO
PRINT N'Creating [dbo].[Train_booking_schedule]'
GO
CREATE TABLE [dbo].[Train_booking_schedule]
(
[idTrain_booking] [int] NOT NULL,
[available_date] [datetime] NULL,
[available_seat_status] [int] NULL,
[Train_idTrain] [int] NOT NULL
)
GO
IF @@ERROR <> 0 SET NOEXEC ON
GO
PRINT N'Creating primary key [PK__Train_bo__945BC01E81926336] on [dbo].[Train_booking_schedule]'
GO
ALTER TABLE [dbo].[Train_booking_schedule] ADD CONSTRAINT [PK__Train_bo__945BC01E81926336] PRIMARY KEY CLUSTERED  ([idTrain_booking])
GO
IF @@ERROR <> 0 SET NOEXEC ON
GO
PRINT N'Creating [dbo].[Train_Ticket]'
GO
CREATE TABLE [dbo].[Train_Ticket]
(
[idTrain] [int] NOT NULL,
[id_Ticket] [int] NOT NULL
)
GO
IF @@ERROR <> 0 SET NOEXEC ON
GO
PRINT N'Creating primary key [PK__Train_Ti__BA821371DDFB0F85] on [dbo].[Train_Ticket]'
GO
ALTER TABLE [dbo].[Train_Ticket] ADD CONSTRAINT [PK__Train_Ti__BA821371DDFB0F85] PRIMARY KEY CLUSTERED  ([idTrain], [id_Ticket])
GO
IF @@ERROR <> 0 SET NOEXEC ON
GO
PRINT N'Creating [dbo].[Train_seats]'
GO





CREATE VIEW [dbo].[Train_seats] AS 
WITH cancellation AS (
SELECT 

	Train.idTrain,
	TrainTypeName,
	seat_number

FROM 
	dbo.Ticket
	INNER JOIN 
		dbo.Train_classType ON Train_classType.idTrain_classType = Ticket.Train_classType_id
	INNER  JOIN 
		dbo.cancellation_status ON cancellation_status.Ticket_idTicket = Ticket.idTicket
	INNER JOIN 
		dbo.Train_Ticket ON Train_Ticket.id_Ticket = Ticket.idTicket
	INNER JOIN 
		dbo.Train ON Train.idTrain = Train_Ticket.idTrain

WHERE
	cancelled_status = 1
GROUP BY

	TrainTypeName,
	Train.idTrain,
	seat_number

),

booking AS (
SELECT 
	train.idTrain,
	seat_number,
	TrainTypeName 

FROM 
	dbo.reservation_status rs
	INNER JOIN 
		dbo.Ticket ON Ticket.idTicket = rs.Ticket_idTicket
	INNER JOIN 
		dbo.Train_Ticket ON Train_Ticket.id_Ticket = Ticket.idTicket
	INNER JOIN 
		dbo.Train_classType ON Train_classType.idTrain_classType = Ticket.Train_classType_id
	INNER JOIN	
		dbo.Train ON Train.idTrain = Train_Ticket.idTrain

WHERE
	rs.reservation_status = 1 
GROUP BY	
	seat_number,
	train.idTrain,
	TrainTypeName 
	
),

T AS (
SELECT 
	seat_number,
	Train.idTrain,
	TrainTypeName
FROM 
	dbo.Train 
	INNER JOIN 
		dbo.Train_Ticket AS TT ON TT.idTrain = train.idTrain
	INNER JOIN 
		dbo.Ticket ON Ticket.idTicket = TT.id_Ticket
	INNER JOIN 
		dbo.Train_classType ON Train_classType.idTrain_classType = Ticket.Train_classType_id
GROUP BY 
	seat_number,
	Train.idTrain,
	TrainTypeName
)

SELECT DISTINCT
	T.idTrain,
	ISNULL(cancel.TrainTypeName, 'No Record') AS [Cancelled seats type],
	ISNULL(STUFF((SELECT  DISTINCT ', '+RTRIM(CAST(cancellation.seat_number AS CHAR))  
       FROM cancellation 
       WHERE cancellation.idTrain = t.idTrain
	   --GROUP BY cancellation.seat_number
       --ORDER BY cancellation.seat_number
       FOR XML PATH('')) , 1, 2, ''), 'No Record') AS cancelled_seats,
	
	ISNULL(book.TrainTypeName, 'No Record') AS [Booked seats type],
	ISNULL(STUFF((SELECT DISTINCT ', '+RTRIM(CAST(booking.seat_number AS CHAR))  
       FROM booking 
       WHERE booking.idTrain = T.idTrain
			AND booking.TrainTypeName= T.TrainTypeName
	   GROUP BY booking.seat_number
       --ORDER BY booking.seat_number
       FOR XML PATH('')) , 1, 2, ''), 'No Record') AS booked_seats
FROM 
	T
	LEFT OUTER JOIN 
	cancellation AS cancel ON cancel.idTrain = T.idTrain
	INNER JOIN 
	booking AS book ON book.idTrain = T.idTrain
					AND book.TrainTypeName = T.TrainTypeName

GO
IF @@ERROR <> 0 SET NOEXEC ON
GO
PRINT N'Creating [dbo].[ticket rating of the worst ]'
GO
CREATE VIEW [dbo].[ticket rating of the worst ] AS
SELECT COUNT (c.service_rating) AS [the worst] ,t.Ticket_Type_idTicket_Type
FROM Comment c
JOIN Ticket t
ON t.Online_account_idOnline_account= c.Online_account_idOnline_account
WHERE c.service_rating =1
GROUP BY t.Ticket_Type_idTicket_Type
GO
IF @@ERROR <> 0 SET NOEXEC ON
GO
PRINT N'Creating [dbo].[ticket rating of the best]'
GO
CREATE VIEW [dbo].[ticket rating of the best] AS
SELECT count(service_rating)  as [the best], t.Ticket_Type_idTicket_Type
FROM Comment c
JOIN Ticket t
ON t.Online_account_idOnline_account= c.Online_account_idOnline_account
WHERE c.service_rating =5
GROUP BY t.Ticket_Type_idTicket_Type
GO
IF @@ERROR <> 0 SET NOEXEC ON
GO
PRINT N'Creating [dbo].[uf_totalsales]'
GO
CREATE FUNCTION [dbo].[uf_totalsales]
(@idt int)
returns int
as 
BEGIN
	DECLARE @Totalsale int
	Select @Totalsale =isnull(sum(TicketTypeFare),0)
	From dbo.Ticket_type a 
	inner join ticket b 
	on a.idticket_type=b.ticket_type_idticket_type
	inner join reservation_status c
	on b.idticket=c.ticket_idticket
	inner join train_ticket d 
	on b.idticket=d.id_ticket
	inner join train e
	on d.idtrain=e.idtrain
	where c.reservation_status='1' 
	and e.idtrain=@idt 
	and b.idticket not in (SELECT ticket_idticket 
						 from cancellation_status f 
						where cancelled_status='1' )
	Return @Totalsale;
END;
GO
IF @@ERROR <> 0 SET NOEXEC ON
GO
PRINT N'Adding constraints to [dbo].[Ticket_Type]'
GO
ALTER TABLE [dbo].[Ticket_Type] ADD CONSTRAINT [CK_ticTyp] CHECK (([idTicket_Type]=(3) OR [idTicket_Type]=(2) OR [idTicket_Type]=(1)))
GO
IF @@ERROR <> 0 SET NOEXEC ON
GO
PRINT N'Adding constraints to [dbo].[Train]'
GO
ALTER TABLE [dbo].[Train] ADD CONSTRAINT [CK_DateGreaterThan1] CHECK (([train_arrtivaltime]>[train_departuretime]))
GO
IF @@ERROR <> 0 SET NOEXEC ON
GO
PRINT N'Adding constraints to [dbo].[Train_classType]'
GO
ALTER TABLE [dbo].[Train_classType] ADD CONSTRAINT [CK_clastyp] CHECK (([idTrain_classType]=(3) OR [idTrain_classType]=(2) OR [idTrain_classType]=(1)))
GO
IF @@ERROR <> 0 SET NOEXEC ON
GO
PRINT N'Adding constraints to [dbo].[cancellation_status]'
GO
ALTER TABLE [dbo].[cancellation_status] ADD CONSTRAINT [CK_DateGreaterThan] CHECK (([cancelled_date]>[requested_date]))
GO
IF @@ERROR <> 0 SET NOEXEC ON
GO
PRINT N'Adding foreign keys to [dbo].[Account_security]'
GO
ALTER TABLE [dbo].[Account_security] ADD CONSTRAINT [fk_Account_security_Online_account1] FOREIGN KEY ([Online_account_idOnline_account]) REFERENCES [dbo].[Online_account] ([idOnline_account])
GO
IF @@ERROR <> 0 SET NOEXEC ON
GO
PRINT N'Adding foreign keys to [dbo].[PassengerAndAddress]'
GO
ALTER TABLE [dbo].[PassengerAndAddress] ADD CONSTRAINT [fk_PassengerAndAddress_Address1] FOREIGN KEY ([Address_idaddress]) REFERENCES [dbo].[Address] ([idaddress])
GO
ALTER TABLE [dbo].[PassengerAndAddress] ADD CONSTRAINT [fk_PassengerAndAddress_Passenger1] FOREIGN KEY ([Passenger_idPassenger]) REFERENCES [dbo].[Passenger] ([idPassenger])
GO
IF @@ERROR <> 0 SET NOEXEC ON
GO
PRINT N'Adding foreign keys to [dbo].[PassengerAndAgent]'
GO
ALTER TABLE [dbo].[PassengerAndAgent] ADD CONSTRAINT [fk_PassengerAndAgent_Agent1] FOREIGN KEY ([Agent_idAgent]) REFERENCES [dbo].[Agent] ([idAgent])
GO
ALTER TABLE [dbo].[PassengerAndAgent] ADD CONSTRAINT [fk_PassengerAndAgent_Passenger1] FOREIGN KEY ([Passenger_idPassenger]) REFERENCES [dbo].[Passenger] ([idPassenger])
GO
IF @@ERROR <> 0 SET NOEXEC ON
GO
PRINT N'Adding foreign keys to [dbo].[Comment]'
GO
ALTER TABLE [dbo].[Comment] ADD CONSTRAINT [fk_Comment_Online_account1] FOREIGN KEY ([Online_account_idOnline_account]) REFERENCES [dbo].[Online_account] ([idOnline_account])
GO
IF @@ERROR <> 0 SET NOEXEC ON
GO
PRINT N'Adding foreign keys to [dbo].[payment]'
GO
ALTER TABLE [dbo].[payment] ADD CONSTRAINT [fk_payment_Online_account1] FOREIGN KEY ([Online_account_idOnline_account]) REFERENCES [dbo].[Online_account] ([idOnline_account])
GO
IF @@ERROR <> 0 SET NOEXEC ON
GO
PRINT N'Adding foreign keys to [dbo].[Register]'
GO
ALTER TABLE [dbo].[Register] ADD CONSTRAINT [fk_Register_Online_account1] FOREIGN KEY ([Online_account_idOnline_account]) REFERENCES [dbo].[Online_account] ([idOnline_account])
GO
ALTER TABLE [dbo].[Register] ADD CONSTRAINT [fk_Register_idPassenger1] FOREIGN KEY ([Passenger_idPassenger]) REFERENCES [dbo].[Passenger] ([idPassenger])
GO
IF @@ERROR <> 0 SET NOEXEC ON
GO
PRINT N'Adding foreign keys to [dbo].[Ticket]'
GO
ALTER TABLE [dbo].[Ticket] ADD CONSTRAINT [fk_Ticket_Online_account1] FOREIGN KEY ([Online_account_idOnline_account]) REFERENCES [dbo].[Online_account] ([idOnline_account])
GO
ALTER TABLE [dbo].[Ticket] ADD CONSTRAINT [fk_Ticket_Train_classTypeName] FOREIGN KEY ([Train_classType_id]) REFERENCES [dbo].[Train_classType] ([idTrain_classType])
GO
ALTER TABLE [dbo].[Ticket] ADD CONSTRAINT [fk_Ticket_Ticket_Type1] FOREIGN KEY ([Ticket_Type_idTicket_Type]) REFERENCES [dbo].[Ticket_Type] ([idTicket_Type])
GO
IF @@ERROR <> 0 SET NOEXEC ON
GO
PRINT N'Adding foreign keys to [dbo].[Route]'
GO
ALTER TABLE [dbo].[Route] ADD CONSTRAINT [fk_Route_Train1] FOREIGN KEY ([idTrain]) REFERENCES [dbo].[Train] ([idTrain])
GO
ALTER TABLE [dbo].[Route] ADD CONSTRAINT [fk_Route_Station1] FOREIGN KEY ([idStation]) REFERENCES [dbo].[Station] ([idStation])
GO
IF @@ERROR <> 0 SET NOEXEC ON
GO
PRINT N'Adding foreign keys to [dbo].[cancellation_status]'
GO
ALTER TABLE [dbo].[cancellation_status] ADD CONSTRAINT [fk_cancellation_status_Ticket1] FOREIGN KEY ([Ticket_idTicket]) REFERENCES [dbo].[Ticket] ([idTicket])
GO
IF @@ERROR <> 0 SET NOEXEC ON
GO
PRINT N'Adding foreign keys to [dbo].[reservation_status]'
GO
ALTER TABLE [dbo].[reservation_status] ADD CONSTRAINT [fk_reservation_status_Ticket1] FOREIGN KEY ([Ticket_idTicket]) REFERENCES [dbo].[Ticket] ([idTicket])
GO
IF @@ERROR <> 0 SET NOEXEC ON
GO
PRINT N'Adding foreign keys to [dbo].[Train_Ticket]'
GO
ALTER TABLE [dbo].[Train_Ticket] ADD CONSTRAINT [fk_Train_Ticket_Ticket1] FOREIGN KEY ([id_Ticket]) REFERENCES [dbo].[Ticket] ([idTicket])
GO
ALTER TABLE [dbo].[Train_Ticket] ADD CONSTRAINT [fk_Train_Ticket_Train1] FOREIGN KEY ([idTrain]) REFERENCES [dbo].[Train] ([idTrain])
GO
IF @@ERROR <> 0 SET NOEXEC ON
GO
PRINT N'Adding foreign keys to [dbo].[Train_booking_schedule]'
GO
ALTER TABLE [dbo].[Train_booking_schedule] ADD CONSTRAINT [fk_Train_booking_schedule_Train1] FOREIGN KEY ([Train_idTrain]) REFERENCES [dbo].[Train] ([idTrain])
GO
IF @@ERROR <> 0 SET NOEXEC ON
GO
COMMIT TRANSACTION
GO
IF @@ERROR <> 0 SET NOEXEC ON
GO
-- This statement writes to the SQL Server Log so SQL Monitor can show this deployment.
IF HAS_PERMS_BY_NAME(N'sys.xp_logevent', N'OBJECT', N'EXECUTE') = 1
BEGIN
    DECLARE @databaseName AS nvarchar(2048), @eventMessage AS nvarchar(2048)
    SET @databaseName = REPLACE(REPLACE(DB_NAME(), N'\', N'\\'), N'"', N'\"')
    SET @eventMessage = N'Redgate SQL Compare: { "deployment": { "description": "Redgate SQL Compare deployed to ' + @databaseName + N'", "database": "' + @databaseName + N'" }}'
    EXECUTE sys.xp_logevent 55000, @eventMessage
END
GO
DECLARE @Success AS BIT
SET @Success = 1
SET NOEXEC OFF
IF (@Success = 1) PRINT 'The database update succeeded'
ELSE BEGIN
	IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION
	PRINT 'The database update failed'
END
GO
