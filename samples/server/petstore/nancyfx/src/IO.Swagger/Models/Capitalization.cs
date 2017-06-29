using System;
using System.Collections.Generic;
using System.IO;
using System.Text;
using Sharpility.Extensions;
using NodaTime;

namespace IO.Swagger.v2.Models
{
    /// <summary>
    /// Capitalization
    /// </summary>
    public sealed class Capitalization:  IEquatable<Capitalization>
    { 
        /// <summary>
        /// SmallCamel
        /// </summary>
        public string SmallCamel { get; private set; }

        /// <summary>
        /// CapitalCamel
        /// </summary>
        public string CapitalCamel { get; private set; }

        /// <summary>
        /// SmallSnake
        /// </summary>
        public string SmallSnake { get; private set; }

        /// <summary>
        /// CapitalSnake
        /// </summary>
        public string CapitalSnake { get; private set; }

        /// <summary>
        /// SCAETHFlowPoints
        /// </summary>
        public string SCAETHFlowPoints { get; private set; }

        /// <summary>
        /// Name of the pet 
        /// </summary>
        public string ATT_NAME { get; private set; }


        /// <summary>
        /// Empty constructor required by some serializers.
        /// Use Capitalization.Builder() for instance creation instead.
        /// </summary>
        [Obsolete]
        public Capitalization()
        {
        }

        private Capitalization(string SmallCamel, string CapitalCamel, string SmallSnake, string CapitalSnake, string SCAETHFlowPoints, string ATT_NAME)
        {
            
            this.SmallCamel = SmallCamel;
            
            this.CapitalCamel = CapitalCamel;
            
            this.SmallSnake = SmallSnake;
            
            this.CapitalSnake = CapitalSnake;
            
            this.SCAETHFlowPoints = SCAETHFlowPoints;
            
            this.ATT_NAME = ATT_NAME;
            
        }

        /// <summary>
        /// Returns builder of Capitalization.
        /// </summary>
        /// <returns>CapitalizationBuilder</returns>
        public static CapitalizationBuilder Builder()
        {
            return new CapitalizationBuilder();
        }

        /// <summary>
        /// Returns CapitalizationBuilder with properties set.
        /// Use it to change properties.
        /// </summary>
        /// <returns>CapitalizationBuilder</returns>
        public CapitalizationBuilder With()
        {
            return Builder()
                .SmallCamel(SmallCamel)
                .CapitalCamel(CapitalCamel)
                .SmallSnake(SmallSnake)
                .CapitalSnake(CapitalSnake)
                .SCAETHFlowPoints(SCAETHFlowPoints)
                .ATT_NAME(ATT_NAME);
        }

        public override string ToString()
        {
            return this.PropertiesToString();
        }

        public override bool Equals(object obj)
        {
            return this.EqualsByProperties(obj);
        }

        public bool Equals(Capitalization other)
        {
            return Equals((object) other);
        }

        public override int GetHashCode()
        {
            return this.PropertiesHash();
        }

        /// <summary>
        /// Implementation of == operator for (Capitalization.
        /// </summary>
        /// <param name="left">Compared (Capitalization</param>
        /// <param name="right">Compared (Capitalization</param>
        /// <returns>true if compared items are equals, false otherwise</returns>
        public static bool operator == (Capitalization left, Capitalization right)
        {
            return Equals(left, right);
        }

        /// <summary>
        /// Implementation of != operator for (Capitalization.
        /// </summary>
        /// <param name="left">Compared (Capitalization</param>
        /// <param name="right">Compared (Capitalization</param>
        /// <returns>true if compared items are not equals, false otherwise</returns>
        public static bool operator != (Capitalization left, Capitalization right)
        {
            return !Equals(left, right);
        }

        /// <summary>
        /// Builder of Capitalization.
        /// </summary>
        public sealed class CapitalizationBuilder
        {
            private string _SmallCamel;
            private string _CapitalCamel;
            private string _SmallSnake;
            private string _CapitalSnake;
            private string _SCAETHFlowPoints;
            private string _ATT_NAME;

            internal CapitalizationBuilder()
            {
                SetupDefaults();
            }

            private void SetupDefaults()
            {
            }

            /// <summary>
            /// Sets value for Capitalization.SmallCamel property.
            /// </summary>
            /// <param name="value">SmallCamel</param>
            public CapitalizationBuilder SmallCamel(string value)
            {
                _SmallCamel = value;
                return this;
            }

            /// <summary>
            /// Sets value for Capitalization.CapitalCamel property.
            /// </summary>
            /// <param name="value">CapitalCamel</param>
            public CapitalizationBuilder CapitalCamel(string value)
            {
                _CapitalCamel = value;
                return this;
            }

            /// <summary>
            /// Sets value for Capitalization.SmallSnake property.
            /// </summary>
            /// <param name="value">SmallSnake</param>
            public CapitalizationBuilder SmallSnake(string value)
            {
                _SmallSnake = value;
                return this;
            }

            /// <summary>
            /// Sets value for Capitalization.CapitalSnake property.
            /// </summary>
            /// <param name="value">CapitalSnake</param>
            public CapitalizationBuilder CapitalSnake(string value)
            {
                _CapitalSnake = value;
                return this;
            }

            /// <summary>
            /// Sets value for Capitalization.SCAETHFlowPoints property.
            /// </summary>
            /// <param name="value">SCAETHFlowPoints</param>
            public CapitalizationBuilder SCAETHFlowPoints(string value)
            {
                _SCAETHFlowPoints = value;
                return this;
            }

            /// <summary>
            /// Sets value for Capitalization.ATT_NAME property.
            /// </summary>
            /// <param name="value">Name of the pet </param>
            public CapitalizationBuilder ATT_NAME(string value)
            {
                _ATT_NAME = value;
                return this;
            }


            /// <summary>
            /// Builds instance of Capitalization.
            /// </summary>
            /// <returns>Capitalization</returns>
            public Capitalization Build()
            {
                Validate();
                return new Capitalization(
                    SmallCamel: _SmallCamel,
                    CapitalCamel: _CapitalCamel,
                    SmallSnake: _SmallSnake,
                    CapitalSnake: _CapitalSnake,
                    SCAETHFlowPoints: _SCAETHFlowPoints,
                    ATT_NAME: _ATT_NAME
                );
            }

            private void Validate()
            { 
            }
        }

        
    }
}
