{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE StandaloneDeriving         #-}

-- | This module exports a lot of 'QC.Arbitrary' instances and some
-- QuickCheck-realted tools.
module Fynder.Test.QuickCheck
  ( forAllArbitrary
  ) where

import           Control.Applicative
import           Control.Lens
import           Control.Monad
import qualified Data.ByteString.Char8                    as B8
import           Data.Function                            (fix)
import           Data.Maybe
import           Data.Semigroup
import qualified Data.Text                                as T
import qualified Data.Time                                as Time
import qualified Data.Vector                              as V
import qualified Data.Vector.Generic                      as VG
import           Test.QuickCheck.Instances                ()
import           Test.QuickCheck                          as QC
import qualified Text.Email.Validate                      as EV

import           Fynder.Types
import           Fynder.Types.Misc.Presence               (Presence(..))
import qualified Fynder.Types.Event.Versioned.Internal    as EvVer
import qualified Fynder.Types.Model.AuthFacebook          as AuthFacebook
import qualified Fynder.Types.Model.AuthTwitter           as AuthTwitter
import qualified Fynder.Types.Model.Business              as Business
import qualified Fynder.Types.Model.BusinessProfileStripe as BusinessProfileStripe
import qualified Fynder.Types.Model.UserProfileBusinessStaff as UserProfileBusinessStaff
import qualified Fynder.Types.Model.Class                 as Class
import qualified Fynder.Types.Model.ClassBooking          as ClassBooking
import qualified Fynder.Types.Model.ClassType             as ClassType
import qualified Fynder.Types.Model.ClassNote             as ClassNote
import qualified Fynder.Types.Model.StripeCustomerProfile as StripeCustomerProfile
import qualified Fynder.Types.Model.Studio                as Studio
import qualified Fynder.Types.Model.Tag                   as Tag
import qualified Fynder.Types.Model.User                  as User

--------------------------------------------------------------------------------
-- Texty

instance QC.Arbitrary Texty where
  arbitrary = fix $ \retry -> do
     t <- T.cons <$> QC.arbitrary <*> QC.arbitrary
     maybe retry return (t ^? texty)
  shrink a = do
     t <- QC.shrink (review texty a)
     maybeToList (t ^? texty)


--------------------------------------------------------------------------------
-- URI

-- | URIs like @(http|https)://[a..z]+\.example\.com/[a..z]+@
instance QC.Arbitrary URI where
  arbitrary = do
     schema <- QC.elements ["http", "https"]
     subdomain <- T.pack <$> QC.resize 5 (QC.listOf1 genLowerCaseAsciiLetter)
     path <- T.pack <$> QC.resize 10000 (QC.listOf1 genLowerCaseAsciiLetter)
     return $ (schema <> "://" <> subdomain <> ".example.com/" <> path) ^?! _URI
  shrink _ = do
     -- TODO: Implement a shrink instance that reduces the URI in length,
     -- possibly while maintaining the same domain but changing the subdomain
     -- and path.
     [] -- no shrinking for now.

--------------------------------------------------------------------------------
-- Gender

instance QC.Arbitrary Gender where
  arbitrary = QC.arbitraryBoundedEnum
  shrink = shrinkBoundedEnum

--------------------------------------------------------------------------------
-- EmailAddress

instance QC.Arbitrary EmailAddress where
  arbitrary = fix $ \retry -> do
    let chars = "abcdefghijklmnopqrstuvwxyz"
    locn <- QC.choose (1,200)
    loc  <- B8.pack <$> replicateM locn (QC.elements chars)
    domn <- QC.choose (1,200)
    dom  <- B8.pack <$> replicateM domn (QC.elements chars)
    maybe retry return $ EV.emailAddress (loc <> "@" <> dom)
  shrink a = do
    (loc', dom') <- QC.shrink (EV.localPart a, EV.domainPart a)
    maybeToList $ EV.emailAddress (loc' <> "@" <> dom')

--------------------------------------------------------------------------------
-- Presence

instance QC.Arbitrary Presence where
  arbitrary = QC.oneof
    [ return Present
    , Absent <$> QC.arbitrary
    ]
  shrink (Absent ma) = Present : map Absent (QC.shrink ma)
  shrink Present     = []


--------------------------------------------------------------------------------
-- Seconds

deriving instance QC.Arbitrary Seconds

--------------------------------------------------------------------------------
-- TZInfoId

instance QC.Arbitrary TZInfoId where
  arbitrary = do
    let tz :: T.Text -> TZInfoId
        tz a = a ^?! texty ^. _Unwrapped
    QC.elements $ map tz
      [ "Europe/Paris"
      , "Europe/London"
      , "America/Argentina/Buenos_Aires"
      , "Europe/Rome"
      ]

--------------------------------------------------------------------------------
-- UserId and User

deriving instance QC.Arbitrary UserId

instance QC.Arbitrary User where
  arbitrary = User
    <$> QC.arbitrary <*> QC.arbitrary <*> QC.arbitrary
    <*> QC.arbitrary <*> QC.arbitrary <*> QC.arbitrary
    <*> QC.arbitrary <*> QC.arbitrary
  shrink (User a b c d e f g h) = do
    (a',b',c',d',e',f',g',h') <- QC.shrink (a,b,c,d,e,f,g,h)
    return $ User a' b' c' d' e' f' g' h'

deriving instance QC.Arbitrary EvVer.UserV0

instance QC.Arbitrary User.ChangedEvent where
  arbitrary = QC.oneof
    [ User.ChangedEmail     <$> QC.arbitrary
    , User.ChangedName      <$> QC.arbitrary
    , User.ChangedTelephone <$> QC.arbitrary
    , User.ChangedAddress   <$> QC.arbitrary
    , User.ChangedBirthday  <$> QC.arbitrary
    , User.ChangedGender    <$> QC.arbitrary
    , User.ChangedTZInfo    <$> QC.arbitrary
    , User.ChangedPicture   <$> QC.arbitrary
    ]
  shrink a = case a of
    User.ChangedEmail     b -> User.ChangedEmail     <$> QC.shrink b
    User.ChangedName      b -> User.ChangedName      <$> QC.shrink b
    User.ChangedTelephone b -> User.ChangedTelephone <$> QC.shrink b
    User.ChangedAddress   b -> User.ChangedAddress   <$> QC.shrink b
    User.ChangedBirthday  b -> User.ChangedBirthday  <$> QC.shrink b
    User.ChangedGender    b -> User.ChangedGender    <$> QC.shrink b
    User.ChangedTZInfo    b -> User.ChangedTZInfo    <$> QC.shrink b
    User.ChangedPicture   b -> User.ChangedPicture   <$> QC.shrink b

deriving instance QC.Arbitrary EvVer.UserChangedEventV0

--------------------------------------------------------------------------------
-- TagId and Tag

deriving instance QC.Arbitrary TagId

instance QC.Arbitrary Tag where
  arbitrary = Tag <$> QC.arbitrary <*> QC.arbitrary
  shrink (Tag a b) = do
    (a',b') <- QC.shrink (a,b)
    return $ Tag a' b'

deriving instance QC.Arbitrary EvVer.TagV0

instance QC.Arbitrary Tag.Target where
  arbitrary = QC.arbitraryBoundedEnum
  shrink = shrinkBoundedEnum

deriving instance QC.Arbitrary EvVer.TagTargetV0

--------------------------------------------------------------------------------
-- BusinessId and Business

deriving instance QC.Arbitrary BusinessId

instance QC.Arbitrary Business where
  arbitrary = Business <$> QC.arbitrary <*> QC.arbitrary
  shrink (Business a b) = do
    (a',b') <- QC.shrink (a,b)
    return $ Business a' b'

deriving instance QC.Arbitrary EvVer.BusinessV0

instance QC.Arbitrary Business.ChangedEvent where
  arbitrary = QC.oneof
    [ Business.ChangedName    <$> QC.arbitrary
    , Business.ChangedAddress <$> QC.arbitrary
    ]
  shrink a = case a of
    Business.ChangedName b    -> Business.ChangedName    <$> QC.shrink b
    Business.ChangedAddress b -> Business.ChangedAddress <$> QC.shrink b

deriving instance QC.Arbitrary EvVer.BusinessChangedEventV0

--------------------------------------------------------------------------------
-- ClassTypeId and ClassType

deriving instance QC.Arbitrary ClassTypeId

instance QC.Arbitrary ClassType where
  arbitrary = ClassType <$> QC.arbitrary <*> QC.arbitrary <*> QC.arbitrary
                        <*> QC.arbitrary
  shrink (ClassType a b c d) = do
     (a',b',c',d') <- QC.shrink (a,b,c,d)
     return $ ClassType a' b' c' d'

deriving instance QC.Arbitrary EvVer.ClassTypeV0

instance QC.Arbitrary ClassType.ChangedEvent where
  arbitrary = QC.oneof
    [ ClassType.ChangedTitle       <$> QC.arbitrary
    , ClassType.ChangedDescription <$> QC.arbitrary
    , ClassType.ChangedTags        <$> QC.arbitrary
    , ClassType.ChangedPicture     <$> QC.arbitrary
    ]
  shrink a = case a of
    ClassType.ChangedTitle       b -> ClassType.ChangedTitle       <$> QC.shrink b
    ClassType.ChangedDescription b -> ClassType.ChangedDescription <$> QC.shrink b
    ClassType.ChangedTags        b -> ClassType.ChangedTags        <$> QC.shrink b
    ClassType.ChangedPicture     b -> ClassType.ChangedPicture     <$> QC.shrink b


deriving instance QC.Arbitrary EvVer.ClassTypeChangedEventV0

--------------------------------------------------------------------------------
-- ClassNoteId and ClassNote

deriving instance QC.Arbitrary ClassNoteId

instance QC.Arbitrary ClassNote where
  arbitrary = ClassNote <$> QC.arbitrary <*> QC.arbitrary
  shrink (ClassNote a b) = do
     (a',b') <- QC.shrink (a,b)
     return $ ClassNote a' b'

deriving instance QC.Arbitrary EvVer.ClassNoteV0

instance QC.Arbitrary ClassNote.ChangedEvent where
  arbitrary = ClassNote.ChangedBody <$> QC.arbitrary
  shrink a = case a of
    ClassNote.ChangedBody b -> ClassNote.ChangedBody <$> QC.shrink b

deriving instance QC.Arbitrary EvVer.ClassNoteChangedEventV0

--------------------------------------------------------------------------------
-- StudioId and Studio

deriving instance QC.Arbitrary StudioId

instance QC.Arbitrary Studio where
  arbitrary = Studio
    <$> QC.arbitrary <*> QC.arbitrary <*> QC.arbitrary
    <*> QC.arbitrary <*> QC.arbitrary
  shrink (Studio a b c d e) = do
    (a',b',c',d',e') <- QC.shrink (a,b,c,d,e)
    return $ Studio a' b' c' d' e'

deriving instance QC.Arbitrary EvVer.StudioV0

instance QC.Arbitrary Studio.ChangedEvent where
  arbitrary = QC.oneof
    [ Studio.ChangedAddress     <$> QC.arbitrary
    , Studio.ChangedDescription <$> QC.arbitrary
    , Studio.ChangedName        <$> QC.arbitrary
    , Studio.ChangedTZInfo      <$> QC.arbitrary
    ]
  shrink a = case a of
    Studio.ChangedAddress     b -> Studio.ChangedAddress     <$> QC.shrink b
    Studio.ChangedDescription b -> Studio.ChangedDescription <$> QC.shrink b
    Studio.ChangedName        b -> Studio.ChangedName        <$> QC.shrink b
    Studio.ChangedTZInfo      b -> Studio.ChangedTZInfo      <$> QC.shrink b

deriving instance QC.Arbitrary EvVer.StudioChangedEventV0

--------------------------------------------------------------------------------
-- TrainerProfileId and TrainerProfile

deriving instance QC.Arbitrary TrainerProfileId

instance QC.Arbitrary TrainerProfile where
  arbitrary = TrainerProfile <$> QC.arbitrary
  shrink (TrainerProfile a) = do
    a' <- QC.shrink a
    return $ TrainerProfile a'

deriving instance QC.Arbitrary EvVer.TrainerProfileV0

--------------------------------------------------------------------------------
-- UserProfileBusinessStaffId and UserProfileBusinessStaff

deriving instance QC.Arbitrary UserProfileBusinessStaffId

instance QC.Arbitrary UserProfileBusinessStaff where
  arbitrary = UserProfileBusinessStaff <$> QC.arbitrary <*> QC.arbitrary <*> QC.arbitrary
                           <*> QC.arbitrary <*> QC.arbitrary <*> QC.arbitrary
                           <*> QC.arbitrary
  shrink (UserProfileBusinessStaff a b c d e f g) = do
    (a',b',c',d',e',f',g') <- QC.shrink (a,b,c,d,e,f,g)
    return $ UserProfileBusinessStaff a' b' c' d' e' f' g'

deriving instance QC.Arbitrary EvVer.UserProfileBusinessStaffV0

instance QC.Arbitrary UserProfileBusinessStaff.ChangedEvent where
  arbitrary = QC.oneof
    [ UserProfileBusinessStaff.ChangedDescription <$> QC.arbitrary
    , UserProfileBusinessStaff.ChangedName <$> QC.arbitrary
    , UserProfileBusinessStaff.ChangedTelephone <$> QC.arbitrary
    , UserProfileBusinessStaff.ChangedEmail <$> QC.arbitrary
    ]
  shrink a = case a of
    UserProfileBusinessStaff.ChangedDescription  b -> UserProfileBusinessStaff.ChangedDescription <$> QC.shrink b
    UserProfileBusinessStaff.ChangedPicture      b -> UserProfileBusinessStaff.ChangedPicture     <$> QC.shrink b
    UserProfileBusinessStaff.ChangedName         b -> UserProfileBusinessStaff.ChangedName        <$> QC.shrink b
    UserProfileBusinessStaff.ChangedTelephone    b -> UserProfileBusinessStaff.ChangedTelephone   <$> QC.shrink b
    UserProfileBusinessStaff.ChangedEmail        b -> UserProfileBusinessStaff.ChangedEmail       <$> QC.shrink b

deriving instance QC.Arbitrary EvVer.UserProfileBusinessStaffChangedEventV0


--------------------------------------------------------------------------------
-- StripeCustomerProfileId and StripeCustomerProfile

deriving instance QC.Arbitrary StripeCustomerProfileId

deriving instance QC.Arbitrary StripeCustomerProfile.StripeCustomerId

instance QC.Arbitrary StripeCustomerProfile where
  arbitrary = StripeCustomerProfile <$> QC.arbitrary <*> QC.arbitrary
  shrink (StripeCustomerProfile a b) = do
    (a',b') <- QC.shrink (a,b)
    return $ StripeCustomerProfile a' b'

deriving instance QC.Arbitrary EvVer.StripeCustomerProfileV0


--------------------------------------------------------------------------------
-- ClassId and Class

deriving instance QC.Arbitrary ClassId

instance QC.Arbitrary Class where
  arbitrary = Class
    <$> QC.arbitrary
    <*> QC.arbitrary
    <*> QC.arbitrary
    <*> QC.arbitrary
    <*> QC.arbitrary
    <*> QC.choose (1, 20)
  shrink (Class a b c d e f) = do
    (a',b',c',d',e',f') <- QC.shrink (a,b,c,d,e,f)
    return $ Class a' b' c' d' e' f'

deriving instance QC.Arbitrary EvVer.ClassV0

instance QC.Arbitrary Class.ChangedEvent where
  arbitrary = QC.oneof
    [ Class.ChangedSlots          <$> QC.choose (1, 20)
    , Class.ChangedType           <$> QC.arbitrary
    , Class.ChangedStudio         <$> QC.arbitrary
    , Class.ChangedTrainerProfile <$> QC.arbitrary
    , do startTs <- QC.arbitrary
         -- duration fixed between 1 and 120 mins
         duration <- fromIntegral . (* 60) <$> QC.choose (1, 120 :: Int)
         let endTs = Time.addUTCTime duration startTs
         return $ Class.ChangedPeriod startTs endTs
    ]
  shrink a = case a of
    Class.ChangedType b -> Class.ChangedType <$> QC.shrink b
    Class.ChangedStudio b -> Class.ChangedStudio <$> QC.shrink b
    Class.ChangedTrainerProfile b -> Class.ChangedTrainerProfile <$> QC.shrink b
    Class.ChangedSlots b -> do
       b' <- QC.shrink b
       guard $ b' > 0
       return $ Class.ChangedSlots b'
    Class.ChangedPeriod b c -> do
       (b',c') <- QC.shrink (b,c)
       guard $ b' < c'
       return $ Class.ChangedPeriod b' c'

deriving instance QC.Arbitrary EvVer.ClassChangedEventV0

--------------------------------------------------------------------------------
-- BusinessProfileStripeId and BusinessProfileStripe

deriving instance QC.Arbitrary BusinessProfileStripeId

deriving instance QC.Arbitrary BusinessProfileStripe.StripeUserId

instance QC.Arbitrary BusinessProfileStripe where
  arbitrary = BusinessProfileStripe
    <$> QC.arbitrary <*> QC.arbitrary <*> QC.arbitrary <*> QC.arbitrary
  shrink (BusinessProfileStripe a b c d) = do
    (a',b',c',d') <- QC.shrink (a,b,c,d)
    return $ BusinessProfileStripe a' b' c' d'

deriving instance QC.Arbitrary EvVer.BusinessProfileStripeV0

instance QC.Arbitrary BusinessProfileStripe.ChangedEvent where
  arbitrary = QC.oneof
    [ BusinessProfileStripe.ChangedAccessToken    <$> QC.arbitrary
    , BusinessProfileStripe.ChangedRefreshToken   <$> QC.arbitrary
    ]
  shrink a = case a of
    BusinessProfileStripe.ChangedAccessToken b ->
       BusinessProfileStripe.ChangedAccessToken <$> QC.shrink b
    BusinessProfileStripe.ChangedRefreshToken b ->
       BusinessProfileStripe.ChangedAccessToken <$> QC.shrink b

deriving instance QC.Arbitrary EvVer.BusinessProfileStripeChangedEventV0

--------------------------------------------------------------------------------
-- CustomerProfileId and CustomerProfile

deriving instance QC.Arbitrary CustomerProfileId

instance QC.Arbitrary CustomerProfile where
  arbitrary = CustomerProfile <$> QC.arbitrary <*> QC.arbitrary
  shrink (CustomerProfile a b) = CustomerProfile <$> QC.shrink a <*> QC.shrink b

deriving instance QC.Arbitrary EvVer.CustomerProfileV0

--------------------------------------------------------------------------------
-- AdminProfileId and AdminProfile

deriving instance QC.Arbitrary AdminProfileId

instance QC.Arbitrary AdminProfile where
  arbitrary = AdminProfile <$> QC.arbitrary
  shrink (AdminProfile a) = do
    a' <- QC.shrink a
    return $ AdminProfile a'

deriving instance QC.Arbitrary EvVer.AdminProfileV0

--------------------------------------------------------------------------------
-- ClassBookingId and ClassBooking

deriving instance QC.Arbitrary ClassBookingId

instance QC.Arbitrary ClassBooking where
  arbitrary = ClassBooking <$> QC.arbitrary <*> QC.arbitrary <*> QC.arbitrary
  shrink (ClassBooking a b c) = do
    (a',b',c') <- QC.shrink (a,b,c)
    return $ ClassBooking a' b' c'

deriving instance QC.Arbitrary EvVer.ClassBookingV0

instance QC.Arbitrary ClassBooking.ChangedEvent where
  arbitrary = QC.oneof
    [ ClassBooking.ChangedCustomerPresence <$> QC.arbitrary
    ]
  shrink a = case a of
    ClassBooking.ChangedCustomerPresence b ->
       ClassBooking.ChangedCustomerPresence <$> QC.shrink b

deriving instance QC.Arbitrary EvVer.ClassBookingChangedEventV0

--------------------------------------------------------------------------------
-- ClassTemplateId and ClassTemplate

deriving instance QC.Arbitrary ClassTemplateId

instance QC.Arbitrary ClassTemplate where
  arbitrary = ClassTemplate
    <$> QC.arbitrary <*> QC.arbitrary <*> QC.arbitrary
    <*> QC.arbitrary <*> QC.arbitrary <*> QC.arbitrary
  shrink (ClassTemplate a b c d e f) = do
    (a',b',c',d',e',f') <- QC.shrink (a,b,c,d,e,f)
    return $ ClassTemplate a' b' c' d' e' f'

deriving instance QC.Arbitrary EvVer.ClassTemplateV0

--------------------------------------------------------------------------------
-- AuthFacebookId and AuthFacebook

deriving instance QC.Arbitrary AuthFacebookId

deriving instance QC.Arbitrary AuthFacebook.FacebookUserId

instance QC.Arbitrary AuthFacebook where
  arbitrary = AuthFacebook <$> QC.arbitrary <*> QC.arbitrary
  shrink (AuthFacebook a b) = do
    (a',b') <- QC.shrink (a,b)
    return $ AuthFacebook a' b'

deriving instance QC.Arbitrary EvVer.AuthFacebookV0

--------------------------------------------------------------------------------
-- AuthTwitterId and AuthTwitter

deriving instance QC.Arbitrary AuthTwitterId

deriving instance QC.Arbitrary AuthTwitter.TwitterId

instance QC.Arbitrary AuthTwitter where
  arbitrary = AuthTwitter <$> QC.arbitrary <*> QC.arbitrary
  shrink (AuthTwitter a b) = do
    (a',b') <- QC.shrink (a,b)
    return $ AuthTwitter a' b'

deriving instance QC.Arbitrary EvVer.AuthTwitterV0

--------------------------------------------------------------------------------
-- ScheduleTemplate

deriving instance QC.Arbitrary ScheduleTemplateId

instance QC.Arbitrary ScheduleTemplate where
  arbitrary = ScheduleTemplate <$> QC.arbitrary <*> QC.arbitrary
  shrink (ScheduleTemplate a b) = do
    (a',b') <- QC.shrink (a,b)
    return $ ScheduleTemplate a' b'

deriving instance QC.Arbitrary EvVer.ScheduleTemplateV0

instance QC.Arbitrary ScheduleTemplatePublisher where
  arbitrary =
     ScheduleTemplatePublisher <$> QC.arbitrary <*> QC.arbitrary <*> QC.arbitrary
  shrink (ScheduleTemplatePublisher a b c) = do
    (a',b',c') <- QC.shrink (a,b,c)
    return $ ScheduleTemplatePublisher a' b' c'

deriving instance QC.Arbitrary EvVer.ScheduleTemplatePublisherV0

--------------------------------------------------------------------------------
-- ModelEvent

instance (QC.Arbitrary k, QC.Arbitrary v, QC.Arbitrary c)
      => QC.Arbitrary (ModelEvent k v c) where
  arbitrary = QC.oneof
    [ ModelCreated <$> QC.arbitrary <*> QC.arbitrary
    , ModelDeleted <$> QC.arbitrary <*> QC.arbitrary
    , ModelChanged <$> QC.arbitrary <*> QC.arbitrary
    ]
  shrink a = case a of
    ModelCreated b c -> uncurry ModelCreated <$> QC.shrink (b, c)
    ModelDeleted b c -> uncurry ModelDeleted <$> QC.shrink (b, c)
    ModelChanged b c -> uncurry ModelChanged <$> QC.shrink (b, c)

deriving instance (QC.Arbitrary k, QC.Arbitrary v, QC.Arbitrary c)
                => QC.Arbitrary (EvVer.ModelEventV0 k v c)

--------------------------------------------------------------------------------
-- Event

deriving instance QC.Arbitrary EventId

instance QC.Arbitrary Event where
  arbitrary = QC.oneof
    [ EvtUserModel <$> QC.arbitrary
    , EvtBusinessModel <$> QC.arbitrary
    , EvtBusinessProfileStripeModel <$> QC.arbitrary
    , EvtStudioModel <$> QC.arbitrary
    , EvtCustomerProfileModel <$> QC.arbitrary
    , EvtTrainerProfileModel <$> QC.arbitrary
    , EvtClassModel <$> QC.arbitrary
    , EvtClassBookingModel <$> QC.arbitrary
    , EvtAdminProfileModel <$> QC.arbitrary
    , EvtClassTypeModel <$> QC.arbitrary
    , EvtClassTemplateModel <$> QC.arbitrary
    , EvtScheduleTemplateModel <$> QC.arbitrary
    , EvtAuthFacebookModel <$> QC.arbitrary
    , EvtAuthTwitterModel <$> QC.arbitrary
    , EvtTagModel <$> QC.arbitrary
    , EvtStripeCustomerProfileModel <$> QC.arbitrary
    , EvtSchedulePublished <$> QC.arbitrary <*> QC.arbitrary
    ]
  shrink a = case a of
    EvtUserModel b                  -> EvtUserModel <$> QC.shrink b
    EvtBusinessModel b              -> EvtBusinessModel <$> QC.shrink b
    EvtBusinessProfileStripeModel b -> EvtBusinessProfileStripeModel <$> QC.shrink b
    EvtUserProfileBusinessStaffModel b -> EvtUserProfileBusinessStaffModel <$> QC.shrink b
    EvtStudioModel b                -> EvtStudioModel <$> QC.shrink b
    EvtCustomerProfileModel b       -> EvtCustomerProfileModel <$> QC.shrink b
    EvtTrainerProfileModel b        -> EvtTrainerProfileModel <$> QC.shrink b
    EvtClassModel b                 -> EvtClassModel <$> QC.shrink b
    EvtClassBookingModel b          -> EvtClassBookingModel <$> QC.shrink b
    EvtAdminProfileModel b          -> EvtAdminProfileModel <$> QC.shrink b
    EvtClassNoteModel b             -> EvtClassNoteModel <$> QC.shrink b
    EvtClassTypeModel b             -> EvtClassTypeModel <$> QC.shrink b
    EvtClassTemplateModel b         -> EvtClassTemplateModel <$> QC.shrink b
    EvtScheduleTemplateModel b      -> EvtScheduleTemplateModel <$> QC.shrink b
    EvtAuthFacebookModel b          -> EvtAuthFacebookModel <$> QC.shrink b
    EvtAuthTwitterModel b           -> EvtAuthTwitterModel <$> QC.shrink b
    EvtTagModel b                   -> EvtTagModel <$> QC.shrink b
    EvtStripeCustomerProfileModel b -> EvtStripeCustomerProfileModel <$> QC.shrink b
    EvtSchedulePublished b c        -> uncurry EvtSchedulePublished <$> QC.shrink (b,c)

deriving instance QC.Arbitrary EvVer.EventV0

--------------------------------------------------------------------------------

instance QC.Arbitrary VersionedEvent where
  arbitrary = toVersionedEvent <$> QC.arbitrary
  shrink a = toVersionedEvent <$> QC.shrink (fromVersionedEvent a)


--------------------------------------------------------------------------------

-- | > 'forAllArbitrary' == 'QC.forAllShrink' 'QC.arbitrary' 'QC.shrink'
forAllArbitrary
  :: (Show a, QC.Testable prop, QC.Arbitrary a)
  => (a -> prop) -> QC.Property
forAllArbitrary = QC.forAllShrink QC.arbitrary QC.shrink

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- Internal stuff

genLowerCaseAsciiLetter :: QC.Gen Char
genLowerCaseAsciiLetter = elementsVector $ V.fromList ['a'..'z']

--------------------------------------------------------------------------------
-- XXX Send all this stuff to QuickCheck

shrinkBoundedEnum :: (Bounded a, Enum a) => a -> [a]
shrinkBoundedEnum a = drop (fromEnum a + 1) [minBound .. maxBound]

instance (QC.Arbitrary a, QC.Arbitrary b, QC.Arbitrary c, QC.Arbitrary d,
          QC.Arbitrary e, QC.Arbitrary f)
       => QC.Arbitrary (a, b, c, d, e, f)
  where
    arbitrary = (,,,,,)
      <$> QC.arbitrary <*> QC.arbitrary <*> QC.arbitrary
      <*> QC.arbitrary <*> QC.arbitrary <*> QC.arbitrary
    shrink (a,b,c,d,e,f) = do
      (a',(b',(c',(d',(e',f'))))) <- QC.shrink (a,(b,(c,(d,(e,f)))))
      return (a',b',c',d',e',f')

instance (QC.Arbitrary a, QC.Arbitrary b, QC.Arbitrary c, QC.Arbitrary d,
          QC.Arbitrary e, QC.Arbitrary f, QC.Arbitrary g)
       => QC.Arbitrary (a, b, c, d, e, f, g)
  where
    arbitrary = (,,,,,,)
      <$> QC.arbitrary <*> QC.arbitrary <*> QC.arbitrary
      <*> QC.arbitrary <*> QC.arbitrary <*> QC.arbitrary
      <*> QC.arbitrary
    shrink (a,b,c,d,e,f,g) = do
      (a',(b',(c',(d',(e',(f',g')))))) <- QC.shrink (a,(b,(c,(d,(e,(f,g))))))
      return (a',b',c',d',e',f',g')

instance (QC.Arbitrary a, QC.Arbitrary b, QC.Arbitrary c, QC.Arbitrary d,
          QC.Arbitrary e, QC.Arbitrary f, QC.Arbitrary g, QC.Arbitrary h)
       => QC.Arbitrary (a, b, c, d, e, f, g, h)
  where
    arbitrary = (,,,,,,,)
      <$> QC.arbitrary <*> QC.arbitrary <*> QC.arbitrary
      <*> QC.arbitrary <*> QC.arbitrary <*> QC.arbitrary
      <*> QC.arbitrary <*> QC.arbitrary
    shrink (a,b,c,d,e,f,g,h) = do
      (a',(b',(c',(d',(e',(f',(g',h'))))))) <-
          QC.shrink (a,(b,(c,(d,(e,(f,(g,h)))))))
      return (a',b',c',d',e',f',g',h')

instance (QC.Arbitrary a, QC.Arbitrary b, QC.Arbitrary c, QC.Arbitrary d,
          QC.Arbitrary e, QC.Arbitrary f, QC.Arbitrary g, QC.Arbitrary h,
          QC.Arbitrary i)
       => QC.Arbitrary (a, b, c, d, e, f, g, h, i)
  where
    arbitrary = (,,,,,,,,)
      <$> QC.arbitrary <*> QC.arbitrary <*> QC.arbitrary
      <*> QC.arbitrary <*> QC.arbitrary <*> QC.arbitrary
      <*> QC.arbitrary <*> QC.arbitrary <*> QC.arbitrary
    shrink (a,b,c,d,e,f,g,h,i) = do
      (a',(b',(c',(d',(e',(f',(g',(h', i')))))))) <-
          QC.shrink (a,(b,(c,(d,(e,(f,(g,(h,i))))))))
      return (a',b',c',d',e',f',g',h',i')


-- | Like 'QC.elements', but takes a 'VG.Vector' instead of a list.
elementsVector :: VG.Vector v a => v a -> QC.Gen a
elementsVector v
  | VG.null v = error "Fynder.TestsAQuickCheck.elementsVector: empty vector"
  | otherwise = VG.indexM v =<< QC.choose (0, VG.length v - 1)
