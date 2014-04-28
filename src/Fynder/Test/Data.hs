{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Main (main) where

import           Control.Applicative              ((<$>), (<*>))
import qualified Control.Exception                           as Ex
import           Control.Monad.Free               (MonadFree)
import           Control.Lens
import qualified Data.Configurator                           as C
import           Data.List                        (zip4)
import           Data.Monoid
import           Data.Time                        (parseTime)
import qualified Data.Text                                   as T
import qualified Database.PostgreSQL.Simple                  as PG
import qualified Options.Applicative                         as OA
import           System.Locale                    (defaultTimeLocale)

import qualified Fynder.Command                              as FC
mport           Fynder.Conf                      (fynderConfFromConfigurator)
import           Fynder.Db.Read
import           Fynder.Types
import qualified Fynder.Types.Model.UserProfileBusinessStaff as UserProfileBusinessStaff
import qualified Fynder.Types.Model.ClassType                as ClassType
import qualified Fynder.Types.Model.Studio                   as Studio
import qualified Fynder.Types.Model.User                     as User
import qualified Fynder.Types.Model.ClassTemplate            as ClassTemplate
import qualified Fynder.Types.Model.Business                 as Business

--------------------------------------------------------------------------------

sampleData :: MonadFree CommandF m => m ()
sampleData = do
    let fynderHQAddress = "77 Leonard St., London, EC2A 4QS, UK" ^?! texty
        fynderHQPhone = "12" ^?! texty
        tzLondon = "Europe/London" ^?! texty ^. _Unwrapped
        gymHQAddress = "42 The Next Web street, London, UK" ^?! texty

    -- Users
    uBen <- userCreate $ User
       { User._email     = "ben@fynder.io" ^? emailAddress
       , User._name      = "Ben Ford" ^?! texty
       , User._telephone = Just fynderHQPhone
       , User._address   = Just fynderHQAddress
       , User._birthday  = Nothing
       , User._gender    = Just Male
       , User._tzinfoId  = tzLondon
       , User._picture   = Nothing
       }
    uBrian <- userCreate $ User
       { User._email     = "brian@fynder.io" ^? emailAddress
       , User._name      = "Brian Schuring" ^?! texty
       , User._telephone = Just fynderHQPhone
       , User._address   = Just fynderHQAddress
       , User._birthday  = Nothing
       , User._gender    = Just Male
       , User._tzinfoId  = tzLondon
       , User._picture   = Nothing
       }
    uMer <- userCreate $ User
       { User._email     = "meredith@fynder.io" ^? emailAddress
       , User._name      = "Meredith Bell" ^?! texty
       , User._telephone = Just fynderHQPhone
       , User._address   = Just fynderHQAddress
       , User._birthday  = Nothing
       , User._gender    = Just Female
       , User._tzinfoId  = tzLondon
       , User._picture   = Nothing
       }
    uKris <- userCreate $ User
       { User._email     = "kris@fynder.io" ^? emailAddress
       , User._name      = "Kris Jenkins" ^?! texty
       , User._telephone = Just fynderHQPhone
       , User._address   = Just fynderHQAddress
       , User._birthday  = Nothing
       , User._gender    = Just Male
       , User._tzinfoId  = tzLondon
       , User._picture   = Nothing
       }
    uEsther <- userCreate $ User
       { User._email     = "esther@example.com" ^? emailAddress
       , User._name      = "Esther" ^?! texty
       , User._telephone = Nothing
       , User._address   = Nothing
       , User._birthday  = Nothing
       , User._gender    = Just Male
       , User._tzinfoId  = tzLondon
       , User._picture   = "http://cms.heartcore.co.uk/trainers/TrainerPhotos/157.jpg" ^? _URI
       }
    uMatt <- userCreate $ User
       { User._email     = "alice@example.com" ^? emailAddress
       , User._name      = "Matt" ^?! texty
       , User._telephone = Nothing
       , User._address   = Nothing
       , User._birthday  = Nothing
       , User._gender    = Just Female
       , User._tzinfoId  = tzLondon
       , User._picture   = "http://cms.heartcore.co.uk/trainers/TrainerPhotos/139.jpg" ^? _URI
       }
    uWill <- userCreate $ User
       { User._email     = "will@example.com" ^? emailAddress
       , User._name      = "William Muscle" ^?! texty
       , User._telephone = Nothing
       , User._address   = Nothing
       , User._birthday  = Nothing
       , User._gender    = Just Male
       , User._tzinfoId  = tzLondon
       , User._picture   = Nothing
       }

    -- Business
    bGym <- businessCreate $ Business
       { Business._name = "Gym" ^?! texty
       , Business._address = gymHQAddress
       }

    -- UserProfileBusinessStaff
    ubsGymEsther <- userProfileBusinessStaffCreate $ UserProfileBusinessStaff
       { UserProfileBusinessStaff._userId      = uEsther
       , UserProfileBusinessStaff._businessId  = bGym
       , UserProfileBusinessStaff._description =
           "Esther graduated from Laine Theatre Arts with a National Dance \
           \Diploma and an I.S.T.D. Dance Teacher Qualification. She has 8 \
           \years experience teaching Pilates to clients of all \
           \abilities." ^? texty
       , UserProfileBusinessStaff._picture     =
           "http://cms.heartcore.co.uk/trainers/TrainerPhotos/157.jpg" ^? _URI
       , UserProfileBusinessStaff._name        = "Esther" ^?! texty
       , UserProfileBusinessStaff._telephone   = Nothing
       , UserProfileBusinessStaff._email       = Nothing
       }
    ubsGymMatt <- userProfileBusinessStaffCreate $ UserProfileBusinessStaff
       { UserProfileBusinessStaff._userId      = uMatt
       , UserProfileBusinessStaff._businessId  = bGym
       , UserProfileBusinessStaff._description =
           "Matt is a native of San Francisco. His former work as a \
           \professional dancer, choreographer and yoga instructor \
           \shine through in his classes, which focus on lengthening \
           \and stretching." ^? texty
       , UserProfileBusinessStaff._picture     =
           "http://cms.heartcore.co.uk/trainers/TrainerPhotos/139.jpg" ^? _URI
       , UserProfileBusinessStaff._name        = "Matt" ^?! texty
       , UserProfileBusinessStaff._telephone   = Nothing
       , UserProfileBusinessStaff._email       = Nothing
       }
    ubsGymWill <- userProfileBusinessStaffCreate $ UserProfileBusinessStaff
       { UserProfileBusinessStaff._userId      = uWill
       , UserProfileBusinessStaff._businessId  = bGym
       , UserProfileBusinessStaff._description = "Kinesiologist" ^? texty
       , UserProfileBusinessStaff._picture     = Nothing
       , UserProfileBusinessStaff._name        = "William Muscle" ^?! texty
       , UserProfileBusinessStaff._telephone   = Nothing
       , UserProfileBusinessStaff._email       = Nothing
       }
    ubsGymBen <- userProfileBusinessStaffCreate $ UserProfileBusinessStaff
       { UserProfileBusinessStaff._userId      = uBen
       , UserProfileBusinessStaff._businessId  = bGym
       , UserProfileBusinessStaff._description = "CTO" ^? texty
       , UserProfileBusinessStaff._picture     = Nothing
       , UserProfileBusinessStaff._name        = "Ben Ford" ^?! texty
       , UserProfileBusinessStaff._telephone   = Nothing
       , UserProfileBusinessStaff._email       = Nothing
       }
    ubsGymBrian <- userProfileBusinessStaffCreate $ UserProfileBusinessStaff
       { UserProfileBusinessStaff._userId      = uBrian
       , UserProfileBusinessStaff._businessId  = bGym
       , UserProfileBusinessStaff._description = "CEO" ^? texty
       , UserProfileBusinessStaff._picture     = Nothing
       , UserProfileBusinessStaff._name        = "Brian Schuring" ^?! texty
       , UserProfileBusinessStaff._telephone   = Nothing
       , UserProfileBusinessStaff._email       = Nothing
       }
    ubsGymMer <- userProfileBusinessStaffCreate $ UserProfileBusinessStaff
       { UserProfileBusinessStaff._userId      = uMer
       , UserProfileBusinessStaff._businessId  = bGym
       , UserProfileBusinessStaff._description = "Head of communications" ^? texty
       , UserProfileBusinessStaff._picture     = Nothing
       , UserProfileBusinessStaff._name        = "Meredith Bell" ^?! texty
       , UserProfileBusinessStaff._telephone   = Nothing
       , UserProfileBusinessStaff._email       = Nothing
       }
    ubsGymKris <- userProfileBusinessStaffCreate $ UserProfileBusinessStaff
       { UserProfileBusinessStaff._userId      = uKris
       , UserProfileBusinessStaff._businessId  = bGym
       , UserProfileBusinessStaff._description = "Front-end engineer" ^? texty
       , UserProfileBusinessStaff._picture     = Nothing
       , UserProfileBusinessStaff._name        = "Kris Jenkins" ^?! texty
       , UserProfileBusinessStaff._telephone   = Nothing
       , UserProfileBusinessStaff._email       = Nothing
       }

    -- CustomerProfiles
    cpGymEsther <- customerProfileCreate $ CustomerProfile uEsther bGym
    cpGymMatt   <- customerProfileCreate $ CustomerProfile uMatt   bGym
    cpGymWill   <- customerProfileCreate $ CustomerProfile uWill   bGym
    cpGymBen    <- customerProfileCreate $ CustomerProfile uBen    bGym
    cpGymBrian  <- customerProfileCreate $ CustomerProfile uBrian  bGym
    cpGymMer    <- customerProfileCreate $ CustomerProfile uMer    bGym
    cpGymKris   <- customerProfileCreate $ CustomerProfile uKris   bGym

    -- AdminProfiles
    apGymEsther <- adminProfileCreate $ AdminProfile ubsGymEsther
    apGymBrian  <- adminProfileCreate $ AdminProfile ubsGymBrian
    apGymBen    <- adminProfileCreate $ AdminProfile ubsGymBen
    apGymMer    <- adminProfileCreate $ AdminProfile ubsGymMer
    apGymKris   <- adminProfileCreate $ AdminProfile ubsGymKris

    -- TrainerProfiles
    tpGymMatt   <- trainerProfileCreate $ TrainerProfile ubsGymMatt
    tpGymEsther <- trainerProfileCreate $ TrainerProfile ubsGymEsther

    -- Studio
    studioGymWest <- studioCreate $ Studio
       { Studio._address         = gymHQAddress
       , Studio._businessId      = bGym
       , Studio._description     = "Headquarters" ^? texty
       , Studio._name            = "West London" ^?! texty
       , Studio._tzinfoId        = tzLondon
       }

    studioGymEast <- studioCreate $ Studio
       { Studio._address         = gymHQAddress
       , Studio._businessId      = bGym
       , Studio._description     = "Our biggest location" ^? texty
       , Studio._name            = "East London" ^?! texty
       , Studio._tzinfoId        = tzLondon
       }

    -- Class types
    ctGymTRX <- classTypeCreate $ ClassType
       { ClassType._businessId  = bGym
       , ClassType._title       = "TRX" ^?! texty
       , ClassType._description =
           "TRX is  a new category of exercise that allows you \
           \to leverage your own body weight and gravity to improve \
           \your strength, stamina, flexibility and balance at the same time. \
           \Our cardio-induced, high-intensity take on this killer workout \
           \maximizes results and fat burn." ^? texty
       , ClassType._picture     =
           "http://m.fynder.io/images/avatar-missing-class.png" ^? _URI
       }
    ctGymPilates <- classTypeCreate $ ClassType
       { ClassType._businessId  = bGym
       , ClassType._title       = "Pilates" ^?! texty
       , ClassType._description =
           "Our most popular class, these reformer-based 55-minute sessions \
           \use core-focused functional training in a highly effective, \
           \high-energy class format to provide a comprehensive full body \
           \workout in the shortest possible timeframe." ^? texty
       , ClassType._picture     =
           "http://m.fynder.io/images/avatar-missing-class.png" ^? _URI
       }

    -- Class template
    ctGymTRXEstherWest <- classTemplateCreate $ ClassTemplate
       { ClassTemplate._name        = "TRX" ^?! texty
       , ClassTemplate._classTypeId = ctGymTRX
       , ClassTemplate._studioId    = studioGymWest
       , ClassTemplate._slots       = 6
       , ClassTemplate._duration    = Seconds (45 * 60)
       , ClassTemplate._trainerId   = tpGymEsther
       }
    ctGymPilatesEstherWest <- classTemplateCreate $ ClassTemplate
       { ClassTemplate._name        = "Pilates" ^?! texty
       , ClassTemplate._classTypeId = ctGymPilates
       , ClassTemplate._studioId    = studioGymWest
       , ClassTemplate._slots       = 6
       , ClassTemplate._duration    = Seconds (45 * 60)
       , ClassTemplate._trainerId   = tpGymEsther
       }

    return ()
