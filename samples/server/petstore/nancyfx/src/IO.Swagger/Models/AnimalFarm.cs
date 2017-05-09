using System;
using System.Collections.Generic;
using System.IO;
using System.Text;
using Sharpility.Extensions;
using NodaTime;

namespace IO.Swagger.v2.Models
{
    /// <summary>
    /// AnimalFarm
    /// </summary>
    public sealed class AnimalFarm: List<Animal>,  IEquatable<AnimalFarm>
    { 

        /// <summary>
        /// Empty constructor required by some serializers.
        /// Use AnimalFarm.Builder() for instance creation instead.
        /// </summary>
        [Obsolete]
        public AnimalFarm() : base()
        {
        }

        private AnimalFarm() : base()
        {
            
        }

        /// <summary>
        /// Returns builder of AnimalFarm.
        /// </summary>
        /// <returns>AnimalFarmBuilder</returns>
        public static new AnimalFarmBuilder Builder()
        {
            return new AnimalFarmBuilder();
        }

        /// <summary>
        /// Returns AnimalFarmBuilder with properties set.
        /// Use it to change properties.
        /// </summary>
        /// <returns>AnimalFarmBuilder</returns>
        public new AnimalFarmBuilder With()
        {
            return Builder()
;
        }

        public override string ToString()
        {
            return this.PropertiesToString();
        }

        public override bool Equals(object obj)
        {
            return this.EqualsByProperties(obj);
        }

        public bool Equals(AnimalFarm other)
        {
            return Equals((object) other);
        }

        public override int GetHashCode()
        {
            return this.PropertiesHash();
        }

        /// <summary>
        /// Implementation of == operator for (AnimalFarm.
        /// </summary>
        /// <param name="left">Compared (AnimalFarm</param>
        /// <param name="right">Compared (AnimalFarm</param>
        /// <returns>true if compared items are equals, false otherwise</returns>
        public static bool operator == (AnimalFarm left, AnimalFarm right)
        {
            return Equals(left, right);
        }

        /// <summary>
        /// Implementation of != operator for (AnimalFarm.
        /// </summary>
        /// <param name="left">Compared (AnimalFarm</param>
        /// <param name="right">Compared (AnimalFarm</param>
        /// <returns>true if compared items are not equals, false otherwise</returns>
        public static bool operator != (AnimalFarm left, AnimalFarm right)
        {
            return !Equals(left, right);
        }

        /// <summary>
        /// Builder of AnimalFarm.
        /// </summary>
        public sealed class AnimalFarmBuilder
        {

            internal AnimalFarmBuilder()
            {
                SetupDefaults();
            }

            private void SetupDefaults()
            {
            }


            /// <summary>
            /// Builds instance of AnimalFarm.
            /// </summary>
            /// <returns>AnimalFarm</returns>
            public AnimalFarm Build()
            {
                Validate();
                return new AnimalFarm(
                );
            }

            private void Validate()
            { 
            }
        }

        
    }
}
